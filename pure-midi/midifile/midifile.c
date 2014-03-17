
/* NOTE: The original source had the byte order of pitch wheel messages (0xe0
   status) wrong, this has hopefully been fixed -- watch out for '- ag' in the
   code.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "midifile.h"

/* Silence clang warnings about undefined switch cases. We should maybe look
   into these some time. */
#pragma clang diagnostic ignored "-Wswitch-enum"

/*
 * Data Types
 */

struct MidiFile
{
	int file_format;
	MidiFileDivisionType_t division_type;
	int resolution;
	int number_of_tracks;
	struct MidiFileTrack *first_track;
	struct MidiFileTrack *last_track;
	struct MidiFileEvent *first_event;
	struct MidiFileEvent *last_event;
};

struct MidiFileTrack
{
	struct MidiFile *midi_file;
	int number;
	int32_t end_tick;
	struct MidiFileTrack *previous_track;
	struct MidiFileTrack *next_track;
	struct MidiFileEvent *first_event;
	struct MidiFileEvent *last_event;
};

struct MidiFileEvent
{
	struct MidiFileTrack *track;
	struct MidiFileEvent *previous_event_in_track;
	struct MidiFileEvent *next_event_in_track;
	struct MidiFileEvent *previous_event_in_file;
	struct MidiFileEvent *next_event_in_file;
	int32_t tick;
	MidiFileEventType_t type;

	union
	{
		struct
		{
			int channel;
			int note;
			int velocity;
		}
		note_off;

		struct
		{
			int channel;
			int note;
			int velocity;
		}
		note_on;

		struct
		{
			int channel;
			int note;
			int amount;
		}
		key_pressure;

		struct
		{
			int channel;
			int number;
			int value;
		}
		control_change;

		struct
		{
			int channel;
			int number;
		}
		program_change;

		struct
		{
			int channel;
			int amount;
		}
		channel_pressure;

		struct
		{
			int channel;
			int value;
		}
		pitch_wheel;

		struct
		{
			int data_length;
			unsigned char *data_buffer;
		}
		sysex;

		struct
		{
			int number;
			int data_length;
			unsigned char *data_buffer;
		}
		meta;
	}
	u;

	int should_be_visited;
};

/*
 * Helpers
 */

static signed short interpret_int16(unsigned char *buffer)
{
	return ((signed short)(buffer[0]) << 8) | (signed short)(buffer[1]);
}

static signed short read_int16(FILE *in)
{
	unsigned char buffer[2];
	fread(buffer, 1, 2, in);
	return interpret_int16(buffer);
}

static void write_int16(FILE *out, signed short value)
{
	unsigned char buffer[2];
	buffer[0] = (unsigned char)((value >> 8) & 0xFF);
	buffer[1] = (unsigned char)(value & 0xFF);
	fwrite(buffer, 1, 2, out);
}

static unsigned short interpret_uint16(unsigned char *buffer)
{
	return ((unsigned short)(buffer[0]) << 8) | (unsigned short)(buffer[1]);
}

static unsigned short read_uint16(FILE *in)
{
	unsigned char buffer[2];
	if (fread(buffer, 1, 2, in)) {}
	return interpret_uint16(buffer);
}

static void write_uint16(FILE *out, unsigned short value)
{
	unsigned char buffer[2];
	buffer[0] = (unsigned char)((value >> 8) & 0xFF);
	buffer[1] = (unsigned char)(value & 0xFF);
	fwrite(buffer, 1, 2, out);
}

static uint32_t interpret_uint32(unsigned char *buffer)
{
	return ((uint32_t)(buffer[0]) << 24) | ((uint32_t)(buffer[1]) << 16) | ((uint32_t)(buffer[2]) << 8) | (uint32_t)(buffer[3]);
}

static uint32_t read_uint32(FILE *in)
{
	unsigned char buffer[4];
	if (fread(buffer, 1, 4, in)) {}
	return interpret_uint32(buffer);
}

static void write_uint32(FILE *out, uint32_t value)
{
	unsigned char buffer[4];
	buffer[0] = (unsigned char)(value >> 24);
	buffer[1] = (unsigned char)((value >> 16) & 0xFF);
	buffer[2] = (unsigned char)((value >> 8) & 0xFF);
	buffer[3] = (unsigned char)(value & 0xFF);
	fwrite(buffer, 1, 4, out);
}

static uint32_t read_variable_length_quantity(FILE *in)
{
	unsigned char b;
	uint32_t value = 0;

	do
	{
		b = fgetc(in);
		value = (value << 7) | (b & 0x7F);
	}
	while ((b & 0x80) == 0x80);

	return value;
}

static void write_variable_length_quantity(FILE *out, uint32_t value)
{
	unsigned char buffer[4];
	int offset = 3;

	while (1)
	{
		buffer[offset] = (unsigned char)(value & 0x7F);
		if (offset < 3) buffer[offset] |= 0x80;
		value >>= 7;
		if ((value == 0) || (offset == 0)) break;
		offset--;
	}

	fwrite(buffer + offset, 1, 4 - offset, out);
}

static void add_event(MidiFileEvent_t new_event)
{
	/* Add in proper sorted order.  Search backwards to optimize for appending. */

	MidiFileEvent_t event;

	for (event = new_event->track->last_event; (event != NULL) && (new_event->tick < event->tick); event = event->previous_event_in_track) {}

	new_event->previous_event_in_track = event;

	if (event == NULL)
	{
		new_event->next_event_in_track = new_event->track->first_event;
		new_event->track->first_event = new_event;
	}
	else
	{
		new_event->next_event_in_track = event->next_event_in_track;
		event->next_event_in_track = new_event;
	}

	if (new_event->next_event_in_track == NULL)
	{
		new_event->track->last_event = new_event;
	}
	else
	{
		new_event->next_event_in_track->previous_event_in_track = new_event;
	}

	for (event = new_event->track->midi_file->last_event; (event != NULL) && (new_event->tick < event->tick); event = event->previous_event_in_file) {}

	new_event->previous_event_in_file = event;

	if (event == NULL)
	{
		new_event->next_event_in_file = new_event->track->midi_file->first_event;
		new_event->track->midi_file->first_event = new_event;
	}
	else
	{
		new_event->next_event_in_file = event->next_event_in_file;
		event->next_event_in_file = new_event;
	}

	if (new_event->next_event_in_file == NULL)
	{
		new_event->track->midi_file->last_event = new_event;
	}
	else
	{
		new_event->next_event_in_file->previous_event_in_file = new_event;
	}

	if (new_event->tick > new_event->track->end_tick) new_event->track->end_tick = new_event->tick;
}

static void remove_event(MidiFileEvent_t event)
{
	if (event->previous_event_in_track == NULL)
	{
		event->track->first_event = event->next_event_in_track;
	}
	else
	{
		event->previous_event_in_track->next_event_in_track = event->next_event_in_track;
	}

	if (event->next_event_in_track == NULL)
	{
		event->track->last_event = event->previous_event_in_track;
	}
	else
	{
		event->next_event_in_track->previous_event_in_track = event->previous_event_in_track;
	}

	if (event->previous_event_in_file == NULL)
	{
		event->track->midi_file->first_event = event->next_event_in_file;
	}
	else
	{
		event->previous_event_in_file->next_event_in_file = event->next_event_in_file;
	}

	if (event->next_event_in_file == NULL)
	{
		event->track->midi_file->last_event = event->previous_event_in_file;
	}
	else
	{
		event->next_event_in_file->previous_event_in_file = event->previous_event_in_file;
	}
}

static void free_events_in_track(MidiFileTrack_t track)
{
	MidiFileEvent_t event, next_event_in_track;

	for (event = track->first_event; event != NULL; event = next_event_in_track)
	{
		next_event_in_track = event->next_event_in_track;

		switch (event->type)
		{
			case MIDI_FILE_EVENT_TYPE_SYSEX:
			{
				free(event->u.sysex.data_buffer);
				break;
			}
			case MIDI_FILE_EVENT_TYPE_META:
			{
				free(event->u.meta.data_buffer);
				break;
			}
			default: ;
		}

		free(event);
	}
}

/*
 * Public API
 */

MidiFile_t MidiFile_load(char *filename)
{
	MidiFile_t midi_file;
	FILE *in;
	unsigned char chunk_id[4], division_type_and_resolution[4];
	int32_t chunk_size, chunk_start;
	int file_format, number_of_tracks, number_of_tracks_read = 0;

	if ((filename == NULL) || ((in = fopen(filename, "rb")) == NULL)) return NULL;

	if (fread(chunk_id, 1, 4, in)) {}
	chunk_size = read_uint32(in);
	chunk_start = ftell(in);

	/* check for the RMID variation on SMF */

	if (memcmp(chunk_id, "RIFF", 4) == 0)
	{
		if (fread(chunk_id, 1, 4, in)) {} /* technically this one is a type id rather than a chunk id, but we'll reuse the buffer anyway */

		if (memcmp(chunk_id, "RMID", 4) != 0)
		{
			fclose(in);
			return NULL;
		}

		if (fread(chunk_id, 1, 4, in)) {}
		chunk_size = read_uint32(in);

		if (memcmp(chunk_id, "data", 4) != 0)
		{
			fclose(in);
			return NULL;
		}

		if (fread(chunk_id, 1, 4, in)) {}
		chunk_size = read_uint32(in);
		chunk_start = ftell(in);
	}

	if (memcmp(chunk_id, "MThd", 4) != 0)
	{
		fclose(in);
		return NULL;
	}

	file_format = read_uint16(in);
	number_of_tracks = read_uint16(in);
	if (fread(division_type_and_resolution, 1, 2, in)) {}

	switch ((signed char)(division_type_and_resolution[0]))
	{
		case -24:
		{
			midi_file = MidiFile_new(file_format, MIDI_FILE_DIVISION_TYPE_SMPTE24, division_type_and_resolution[1]);
			break;
		}
		case -25:
		{
			midi_file = MidiFile_new(file_format, MIDI_FILE_DIVISION_TYPE_SMPTE25, division_type_and_resolution[1]);
			break;
		}
		case -29:
		{
			midi_file = MidiFile_new(file_format, MIDI_FILE_DIVISION_TYPE_SMPTE30DROP, division_type_and_resolution[1]);
			break;
		}
		case -30:
		{
			midi_file = MidiFile_new(file_format, MIDI_FILE_DIVISION_TYPE_SMPTE30, division_type_and_resolution[1]);
			break;
		}
		default:
		{
			midi_file = MidiFile_new(file_format, MIDI_FILE_DIVISION_TYPE_PPQ, interpret_uint16(division_type_and_resolution));
			break;
		}
	}

	/* forwards compatibility:  skip over any extra header data */
	fseek(in, chunk_start + chunk_size, SEEK_SET);

	while (number_of_tracks_read < number_of_tracks)
	{
		if (fread(chunk_id, 1, 4, in)) {}
		chunk_size = read_uint32(in);
		chunk_start = ftell(in);

		if (memcmp(chunk_id, "MTrk", 4) == 0)
		{
			MidiFileTrack_t track = MidiFile_createTrack(midi_file);
			int32_t tick, previous_tick = 0;
			unsigned char status, running_status = 0;
			int at_end_of_track = 0;

			while ((ftell(in) < chunk_start + chunk_size) && !at_end_of_track)
			{
				tick = read_variable_length_quantity(in) + previous_tick;
				previous_tick = tick;

				status = fgetc(in);

				if ((status & 0x80) == 0x00)
				{
					status = running_status;
					fseek(in, -1, SEEK_CUR);
				}
				else
				{
					running_status = status;
				}

				switch (status & 0xF0)
				{
					case 0x80:
					{
						int channel = status & 0x0F;
						int note = fgetc(in);
						int velocity = fgetc(in);
						MidiFileTrack_createNoteOffEvent(track, tick, channel, note, velocity);
						break;
					}
					case 0x90:
					{
						int channel = status & 0x0F;
						int note = fgetc(in);
						int velocity = fgetc(in);
						MidiFileTrack_createNoteOnEvent(track, tick, channel, note, velocity);
						break;
					}
					case 0xA0:
					{
						int channel = status & 0x0F;
						int note = fgetc(in);
						int amount = fgetc(in);
						MidiFileTrack_createKeyPressureEvent(track, tick, channel, note, amount);
						break;
					}
					case 0xB0:
					{
						int channel = status & 0x0F;
						int number = fgetc(in);
						int value = fgetc(in);
						MidiFileTrack_createControlChangeEvent(track, tick, channel, number, value);
						break;
					}
					case 0xC0:
					{
						int channel = status & 0x0F;
						int number = fgetc(in);
						MidiFileTrack_createProgramChangeEvent(track, tick, channel, number);
						break;
					}
					case 0xD0:
					{
						int channel = status & 0x0F;
						int amount = fgetc(in);
						MidiFileTrack_createChannelPressureEvent(track, tick, channel, amount);
						break;
					}
					case 0xE0:
					{
						int channel = status & 0x0F;
						int value = fgetc(in);
						// fixed reversed byte order -- ag
						value = value | (fgetc(in) << 7);
						MidiFileTrack_createPitchWheelEvent(track, tick, channel, value);
						break;
					}
					case 0xF0:
					{
						switch (status)
						{
							case 0xF0:
							case 0xF7:
							{
								int data_length = read_variable_length_quantity(in) + 1;
								unsigned char *data_buffer = malloc(data_length);
								data_buffer[0] = status;
								if (fread(data_buffer + 1, 1, data_length - 1, in)) {}
								MidiFileTrack_createSysexEvent(track, tick, data_length, data_buffer);
								free(data_buffer);
								break;
							}
							case 0xFF:
							{
								int number = fgetc(in);
								int data_length = read_variable_length_quantity(in);
								unsigned char *data_buffer = malloc(data_length);
								if (fread(data_buffer, 1, data_length, in)) {}

								if (number == 0x2F)
								{
									MidiFileTrack_setEndTick(track, tick);
									at_end_of_track = 1;
								}
								else
								{
									MidiFileTrack_createMetaEvent(track, tick, number, data_length, data_buffer);
								}

								free(data_buffer);
								break;
							}
						}

						break;
					}
				}
			}

			number_of_tracks_read++;
		}

		/* forwards compatibility:  skip over any unrecognized chunks, or extra data at the end of tracks */
		fseek(in, chunk_start + chunk_size, SEEK_SET);
	}

	fclose(in);
	return midi_file;
}

int MidiFile_save(MidiFile_t midi_file, const char* filename)
{
	FILE *out;
	MidiFileTrack_t track;

	if ((midi_file == NULL) || (filename == NULL) || ((out = fopen(filename, "wb")) == NULL)) return -1;

	fwrite("MThd", 1, 4, out);
	write_uint32(out, 6);
	write_uint16(out, (unsigned short)(MidiFile_getFileFormat(midi_file)));
	write_uint16(out, (unsigned short)(MidiFile_getNumberOfTracks(midi_file)));

	switch (MidiFile_getDivisionType(midi_file))
	{
		case MIDI_FILE_DIVISION_TYPE_PPQ:
		{
			write_uint16(out, (unsigned short)(MidiFile_getResolution(midi_file)));
			break;
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE24:
		{
			fputc(-24, out);
			fputc(MidiFile_getResolution(midi_file), out);
			break;
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE25:
		{
			fputc(-25, out);
			fputc(MidiFile_getResolution(midi_file), out);
			break;
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30DROP:
		{
			fputc(-29, out);
			fputc(MidiFile_getResolution(midi_file), out);
			break;
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30:
		{
			fputc(-30, out);
			fputc(MidiFile_getResolution(midi_file), out);
			break;
		}
		default: ;
	}

	for (track = MidiFile_getFirstTrack(midi_file); track != NULL; track = MidiFileTrack_getNextTrack(track))
	{
		MidiFileEvent_t event;
		int32_t track_size_offset, track_start_offset, track_end_offset, tick, previous_tick;

		fwrite("MTrk", 1, 4, out);

		track_size_offset = ftell(out);
		write_uint32(out, 0);

		track_start_offset = ftell(out);

		previous_tick = 0;

		for (event = MidiFileTrack_getFirstEvent(track); event != NULL; event = MidiFileEvent_getNextEventInTrack(event))
		{
			tick = MidiFileEvent_getTick(event);
			write_variable_length_quantity(out, tick - previous_tick);

			switch (MidiFileEvent_getType(event))
			{
				case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
				{
					fputc(0x80 | (MidiFileNoteOffEvent_getChannel(event) & 0x0F), out);
					fputc(MidiFileNoteOffEvent_getNote(event) & 0x7F, out);
					fputc(MidiFileNoteOffEvent_getVelocity(event) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_NOTE_ON:
				{
					fputc(0x90 | (MidiFileNoteOnEvent_getChannel(event) & 0x0F), out);
					fputc(MidiFileNoteOnEvent_getNote(event) & 0x7F, out);
					fputc(MidiFileNoteOnEvent_getVelocity(event) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_KEY_PRESSURE:
				{
					fputc(0xA0 | (MidiFileKeyPressureEvent_getChannel(event) & 0x0F), out);
					fputc(MidiFileKeyPressureEvent_getNote(event) & 0x7F, out);
					fputc(MidiFileKeyPressureEvent_getAmount(event) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE:
				{
					fputc(0xB0 | (MidiFileControlChangeEvent_getChannel(event) & 0x0F), out);
					fputc(MidiFileControlChangeEvent_getNumber(event) & 0x7F, out);
					fputc(MidiFileControlChangeEvent_getValue(event) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE:
				{
					fputc(0xC0 | (MidiFileProgramChangeEvent_getChannel(event) & 0x0F), out);
					fputc(MidiFileProgramChangeEvent_getNumber(event) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE:
				{
					fputc(0xD0 | (MidiFileChannelPressureEvent_getChannel(event) & 0x0F), out);
					fputc(MidiFileChannelPressureEvent_getAmount(event) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_PITCH_WHEEL:
				{
					int value = MidiFilePitchWheelEvent_getValue(event);
					fputc(0xE0 | (MidiFilePitchWheelEvent_getChannel(event) & 0x0F), out);
					// fixed reversed byte order -- ag
					fputc(value & 0x7F, out);
					fputc((value >> 7) & 0x7F, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_SYSEX:
				{
					int data_length = MidiFileSysexEvent_getDataLength(event);
					unsigned char *data = MidiFileSysexEvent_getData(event);
					fputc(data[0], out);
					write_variable_length_quantity(out, data_length - 1);
					fwrite(data + 1, 1, data_length - 1, out);
					break;
				}
				case MIDI_FILE_EVENT_TYPE_META:
				{
					int data_length = MidiFileMetaEvent_getDataLength(event);
					unsigned char *data = MidiFileMetaEvent_getData(event);
					fputc(0xFF, out);
					fputc(MidiFileMetaEvent_getNumber(event) & 0x7F, out);
					write_variable_length_quantity(out, data_length);
					fwrite(data, 1, data_length, out);
					break;
				}
				default: ;
			}

			previous_tick = tick;
		}

		write_variable_length_quantity(out, MidiFileTrack_getEndTick(track) - previous_tick);
		fwrite("\xFF\x2F\x00", 1, 3, out);

		track_end_offset = ftell(out);

		fseek(out, track_size_offset, SEEK_SET);
		write_uint32(out, track_end_offset - track_start_offset);

		fseek(out, track_end_offset, SEEK_SET);
	}

	fclose(out);
	return 0;
}

MidiFile_t MidiFile_new(int file_format, MidiFileDivisionType_t division_type, int resolution)
{
	MidiFile_t midi_file = (MidiFile_t)(malloc(sizeof(struct MidiFile)));
	midi_file->file_format = file_format;
	midi_file->division_type = division_type;
	midi_file->resolution = resolution;
	midi_file->number_of_tracks = 0;
	midi_file->first_track = NULL;
	midi_file->last_track = NULL;
	midi_file->first_event = NULL;
	midi_file->last_event = NULL;
	return midi_file;
}

int MidiFile_free(MidiFile_t midi_file)
{
	MidiFileTrack_t track, next_track;

	if (midi_file == NULL) return -1;

	for (track = midi_file->first_track; track != NULL; track = next_track)
	{
		next_track = track->next_track;
		free_events_in_track(track);
		free(track);
	}

	free(midi_file);
	return 0;
}

int MidiFile_getFileFormat(MidiFile_t midi_file)
{
	if (midi_file == NULL) return -1;
	return midi_file->file_format;
}

int MidiFile_setFileFormat(MidiFile_t midi_file, int file_format)
{
	if (midi_file == NULL) return -1;
	midi_file->file_format = file_format;
	return 0;
}

MidiFileDivisionType_t MidiFile_getDivisionType(MidiFile_t midi_file)
{
	if (midi_file == NULL) return MIDI_FILE_DIVISION_TYPE_INVALID;
	return midi_file->division_type;
}

int MidiFile_setDivisionType(MidiFile_t midi_file, MidiFileDivisionType_t division_type)
{
	if (midi_file == NULL) return -1;
	midi_file->division_type = division_type;
	return 0;
}

int MidiFile_getResolution(MidiFile_t midi_file)
{
	if (midi_file == NULL) return -1;
	return midi_file->resolution;
}

int MidiFile_setResolution(MidiFile_t midi_file, int resolution)
{
	if (midi_file == NULL) return -1;
	midi_file->resolution = resolution;
	return 0;
}

MidiFileTrack_t MidiFile_createTrack(MidiFile_t midi_file)
{
	MidiFileTrack_t new_track;

	if (midi_file == NULL) return NULL;

	new_track = (MidiFileTrack_t)(malloc(sizeof(struct MidiFileTrack)));
	new_track->midi_file = midi_file;
	new_track->number = midi_file->number_of_tracks;
	new_track->end_tick = 0;
	new_track->previous_track = midi_file->last_track;
	new_track->next_track = NULL;
	midi_file->last_track = new_track;

	if (new_track->previous_track == NULL)
	{
		midi_file->first_track = new_track;
	}
	else
	{
		new_track->previous_track->next_track = new_track;
	}

	(midi_file->number_of_tracks)++;

	new_track->first_event = NULL;
	new_track->last_event = NULL;

	return new_track;
}

int MidiFile_getNumberOfTracks(MidiFile_t midi_file)
{
	if (midi_file == NULL) return -1;
	return midi_file->number_of_tracks;
}

MidiFileTrack_t MidiFile_getTrackByNumber(MidiFile_t midi_file, int number, int create)
{
	int current_track_number;
	MidiFileTrack_t track = NULL;

	for (current_track_number = 0; current_track_number <= number; current_track_number++)
	{
		if (track == NULL)
		{
			track = MidiFile_getFirstTrack(midi_file);
		}
		else
		{
			track = MidiFileTrack_getNextTrack(track);
		}

		if ((track == NULL) && create)
		{
			track = MidiFile_createTrack(midi_file);
		}
	}

	return track;
}

MidiFileTrack_t MidiFile_getFirstTrack(MidiFile_t midi_file)
{
	if (midi_file == NULL) return NULL;
	return midi_file->first_track;
}

MidiFileTrack_t MidiFile_getLastTrack(MidiFile_t midi_file)
{
	if (midi_file == NULL) return NULL;
	return midi_file->last_track;
}

MidiFileEvent_t MidiFile_getFirstEvent(MidiFile_t midi_file)
{
	if (midi_file == NULL) return NULL;
	return midi_file->first_event;
}

MidiFileEvent_t MidiFile_getLastEvent(MidiFile_t midi_file)
{
	if (midi_file == NULL) return NULL;
	return midi_file->last_event;
}

int MidiFile_visitEvents(MidiFile_t midi_file, MidiFileEventVisitorCallback_t visitor_callback, void *user_data)
{
	MidiFileEvent_t event, next_event;

	if ((midi_file == NULL) || (visitor_callback == NULL)) return -1;

	for (event = MidiFile_getFirstEvent(midi_file); event != NULL; event = MidiFileEvent_getNextEventInFile(event)) event->should_be_visited = 1;

	for (event = MidiFile_getFirstEvent(midi_file); event != NULL; event = next_event)
	{
		next_event = MidiFileEvent_getNextEventInFile(event);

		if (event->should_be_visited)
		{
			event->should_be_visited = 0;
			(*visitor_callback)(event, user_data);
		}
	}

	return 0;
}

float MidiFile_getTimeFromTick(MidiFile_t midi_file, int32_t tick)
{
	switch (MidiFile_getDivisionType(midi_file))
	{
		case MIDI_FILE_DIVISION_TYPE_PPQ:
		{
			MidiFileTrack_t conductor_track = MidiFile_getFirstTrack(midi_file);
			MidiFileEvent_t event;
			float tempo_event_time = 0.0;
			int32_t tempo_event_tick = 0;
			float tempo = 120.0;

			for (event = MidiFileTrack_getFirstEvent(conductor_track); (event != NULL) && (MidiFileEvent_getTick(event) < tick); event = MidiFileEvent_getNextEventInTrack(event))
			{
				if (MidiFileEvent_isTempoEvent(event))
				{
					tempo_event_time += (((float)(MidiFileEvent_getTick(event) - tempo_event_tick)) / MidiFile_getResolution(midi_file) / (tempo / 60));
					tempo_event_tick = MidiFileEvent_getTick(event);
					tempo = MidiFileTempoEvent_getTempo(event);
				}
			}

			return tempo_event_time + (((float)(tick - tempo_event_tick)) / MidiFile_getResolution(midi_file) / (tempo / 60));
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE24:
		{
			return (float)(tick) / (MidiFile_getResolution(midi_file) * 24.0);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE25:
		{
			return (float)(tick) / (MidiFile_getResolution(midi_file) * 25.0);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30DROP:
		{
			return (float)(tick) / (MidiFile_getResolution(midi_file) * 29.97);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30:
		{
			return (float)(tick) / (MidiFile_getResolution(midi_file) * 30.0);
		}
		default:
		{
			return -1;
		}
	}
}

int32_t MidiFile_getTickFromTime(MidiFile_t midi_file, float time)
{
	switch (MidiFile_getDivisionType(midi_file))
	{
		case MIDI_FILE_DIVISION_TYPE_PPQ:
		{
			MidiFileTrack_t conductor_track = MidiFile_getFirstTrack(midi_file);
			MidiFileEvent_t event;
			float tempo_event_time = 0.0;
			int32_t tempo_event_tick = 0;
			float tempo = 120.0;

			for (event = MidiFileTrack_getFirstEvent(conductor_track); event != NULL; event = MidiFileEvent_getNextEventInTrack(event))
			{
				if (MidiFileEvent_isTempoEvent(event))
				{
					float next_tempo_event_time = tempo_event_time + (((float)(MidiFileEvent_getTick(event) - tempo_event_tick)) / MidiFile_getResolution(midi_file) / (tempo / 60));
					if (next_tempo_event_time >= time) break;
					tempo_event_time = next_tempo_event_time;
					tempo_event_tick = MidiFileEvent_getTick(event);
					tempo = MidiFileTempoEvent_getTempo(event);
				}
			}

			return tempo_event_tick + ((time - tempo_event_time) * (tempo / 60) * MidiFile_getResolution(midi_file));
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE24:
		{
			return (int32_t)(time * MidiFile_getResolution(midi_file) * 24.0);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE25:
		{
			return (int32_t)(time * MidiFile_getResolution(midi_file) * 25.0);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30DROP:
		{
			return (int32_t)(time * MidiFile_getResolution(midi_file) * 29.97);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30:
		{
			return (int32_t)(time * MidiFile_getResolution(midi_file) * 30.0);
		}
		default:
		{
			return -1;
		}
	}
}

float MidiFile_getBeatFromTick(MidiFile_t midi_file, int32_t tick)
{
	switch (MidiFile_getDivisionType(midi_file))
	{
		case MIDI_FILE_DIVISION_TYPE_PPQ:
		{
			return (float)(tick) / MidiFile_getResolution(midi_file);
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE24:
		{
			return -1.0; /* TODO */
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE25:
		{
			return -1.0; /* TODO */
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30DROP:
		{
			return -1.0; /* TODO */
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30:
		{
			return -1.0; /* TODO */
		}
		default:
		{
			return -1.0;
		}
	}
}

int32_t MidiFile_getTickFromBeat(MidiFile_t midi_file, float beat)
{
	switch (MidiFile_getDivisionType(midi_file))
	{
		case MIDI_FILE_DIVISION_TYPE_PPQ:
		{
			return (int32_t)(beat * MidiFile_getResolution(midi_file));
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE24:
		{
			return -1; /* TODO */
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE25:
		{
			return -1; /* TODO */
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30DROP:
		{
			return -1; /* TODO */
		}
		case MIDI_FILE_DIVISION_TYPE_SMPTE30:
		{
			return -1; /* TODO */
		}
		default:
		{
			return -1;
		}
	}
}

int MidiFileTrack_delete(MidiFileTrack_t track)
{
	MidiFileTrack_t subsequent_track;

	if (track == NULL) return -1;

	for (subsequent_track = track->next_track; subsequent_track != NULL; subsequent_track = subsequent_track->next_track)
	{
		(subsequent_track->number)--;
	}
	
	(track->midi_file->number_of_tracks)--;

	if (track->previous_track == NULL)
	{
		track->midi_file->first_track = track->next_track;
	}
	else
	{
		track->previous_track->next_track = track->next_track;
	}

	if (track->next_track == NULL)
	{
		track->midi_file->last_track = track->previous_track;
	}
	else
	{
		track->next_track->previous_track = track->previous_track;
	}

	free_events_in_track(track);
	free(track);
	return 0;
}

MidiFile_t MidiFileTrack_getMidiFile(MidiFileTrack_t track)
{
	if (track == NULL) return NULL;
	return track->midi_file;
}

int MidiFileTrack_getNumber(MidiFileTrack_t track)
{
	if (track == NULL) return -1;
	return track->number;
}

int32_t MidiFileTrack_getEndTick(MidiFileTrack_t track)
{
	if (track == NULL) return -1;
	return track->end_tick;
}

int MidiFileTrack_setEndTick(MidiFileTrack_t track, int32_t end_tick)
{
	if ((track == NULL) || ((track->last_event != NULL) && (end_tick < track->last_event->tick))) return -1;
	track->end_tick = end_tick;
	return 0;
}

MidiFileTrack_t MidiFileTrack_createTrackBefore(MidiFileTrack_t track)
{
	MidiFileTrack_t new_track, subsequent_track;

	if (track == NULL) return NULL;

	new_track = (MidiFileTrack_t)(malloc(sizeof(struct MidiFileTrack)));
	new_track->midi_file = track->midi_file;
	new_track->number = track->number;
	new_track->end_tick = 0;
	new_track->previous_track = track->previous_track;
	new_track->next_track = track;
	track->previous_track = new_track;

	if (new_track->previous_track == NULL)
	{
		track->midi_file->first_track = new_track;
	}
	else
	{
		new_track->previous_track->next_track = new_track;
	}

	for (subsequent_track = track; subsequent_track != NULL; subsequent_track = subsequent_track->next_track)
	{
		(subsequent_track->number)++;
	}

	new_track->first_event = NULL;
	new_track->last_event = NULL;

	return new_track;
}

MidiFileTrack_t MidiFileTrack_getPreviousTrack(MidiFileTrack_t track)
{
	if (track == NULL) return NULL;
	return track->previous_track;
}

MidiFileTrack_t MidiFileTrack_getNextTrack(MidiFileTrack_t track)
{
	if (track == NULL) return NULL;
	return track->next_track;
}

MidiFileEvent_t MidiFileTrack_createNoteOffEvent(MidiFileTrack_t track, int32_t tick, int channel, int note, int velocity)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_NOTE_OFF;
	new_event->u.note_off.channel = channel;
	new_event->u.note_off.note = note;
	new_event->u.note_off.velocity = velocity;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createNoteOnEvent(MidiFileTrack_t track, int32_t tick, int channel, int note, int velocity)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_NOTE_ON;
	new_event->u.note_on.channel = channel;
	new_event->u.note_on.note = note;
	new_event->u.note_on.velocity = velocity;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createKeyPressureEvent(MidiFileTrack_t track, int32_t tick, int channel, int note, int amount)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_KEY_PRESSURE;
	new_event->u.key_pressure.channel = channel;
	new_event->u.key_pressure.note = note;
	new_event->u.key_pressure.amount = amount;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createControlChangeEvent(MidiFileTrack_t track, int32_t tick, int channel, int number, int value)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE;
	new_event->u.control_change.channel = channel;
	new_event->u.control_change.number = number;
	new_event->u.control_change.value = value;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createProgramChangeEvent(MidiFileTrack_t track, int32_t tick, int channel, int number)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE;
	new_event->u.program_change.channel = channel;
	new_event->u.program_change.number = number;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createChannelPressureEvent(MidiFileTrack_t track, int32_t tick, int channel, int amount)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE;
	new_event->u.channel_pressure.channel = channel;
	new_event->u.channel_pressure.amount = amount;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createPitchWheelEvent(MidiFileTrack_t track, int32_t tick, int channel, int value)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_PITCH_WHEEL;
	new_event->u.pitch_wheel.channel = channel;
	new_event->u.pitch_wheel.value = value;
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createSysexEvent(MidiFileTrack_t track, int32_t tick, int data_length, unsigned char *data_buffer)
{
	MidiFileEvent_t new_event;

	if ((track == NULL) || (data_length < 1) || (data_buffer == NULL)) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_SYSEX;
	new_event->u.sysex.data_length = data_length;
	new_event->u.sysex.data_buffer = malloc(data_length);
	memcpy(new_event->u.sysex.data_buffer, data_buffer, data_length);
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createMetaEvent(MidiFileTrack_t track, int32_t tick, int number, int data_length, unsigned char *data_buffer)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	new_event->type = MIDI_FILE_EVENT_TYPE_META;
	new_event->u.meta.number = number;
	new_event->u.meta.data_length = data_length;
	new_event->u.meta.data_buffer = malloc(data_length);
	memcpy(new_event->u.meta.data_buffer, data_buffer, data_length);
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_createNoteStartAndEndEvents(MidiFileTrack_t track, int32_t start_tick, int32_t end_tick, int channel, int note, int start_velocity, int end_velocity)
{
	MidiFileEvent_t start_event = MidiFileTrack_createNoteOnEvent(track, start_tick, channel, note, start_velocity);
	MidiFileEvent_t end_event = MidiFileTrack_createNoteOffEvent(track, end_tick, channel, note, end_velocity);
	return start_event;
}

MidiFileEvent_t MidiFileTrack_createTempoEvent(MidiFileTrack_t track, int32_t tick, float tempo)
{
	int32_t midi_tempo = 60000000 / tempo;
	unsigned char buffer[3];
	buffer[0] = (midi_tempo >> 16) & 0xFF;
	buffer[1] = (midi_tempo >> 8) & 0xFF;
	buffer[2] = midi_tempo & 0xFF;
	return MidiFileTrack_createMetaEvent(track, tick, 0x51, 3, buffer);
}

MidiFileEvent_t MidiFileTrack_createVoiceEvent(MidiFileTrack_t track, int32_t tick, uint32_t data)
{
	MidiFileEvent_t new_event;

	if (track == NULL) return NULL;

	new_event = (MidiFileEvent_t)(malloc(sizeof(struct MidiFileEvent)));
	new_event->track = track;
	new_event->tick = tick;
	MidiFileVoiceEvent_setData(new_event, data);
	new_event->should_be_visited = 0;
	add_event(new_event);

	return new_event;
}

MidiFileEvent_t MidiFileTrack_getFirstEvent(MidiFileTrack_t track)
{
	if (track == NULL) return NULL;
	return track->first_event;
}

MidiFileEvent_t MidiFileTrack_getLastEvent(MidiFileTrack_t track)
{
	if (track == NULL) return NULL;
	return track->last_event;
}

int MidiFileTrack_visitEvents(MidiFileTrack_t track, MidiFileEventVisitorCallback_t visitor_callback, void *user_data)
{
	MidiFileEvent_t event, next_event;

	if ((track == NULL) || (visitor_callback == NULL)) return -1;

	for (event = MidiFileTrack_getFirstEvent(track); event != NULL; event = MidiFileEvent_getNextEventInTrack(event)) event->should_be_visited = 1;

	for (event = MidiFileTrack_getFirstEvent(track); event != NULL; event = next_event)
	{
		next_event = MidiFileEvent_getNextEventInTrack(event);

		if (event->should_be_visited)
		{
			event->should_be_visited = 0;
			(*visitor_callback)(event, user_data);
		}
	}

	return 0;
}

int MidiFileEvent_delete(MidiFileEvent_t event)
{
	if (event == NULL) return -1;
	remove_event(event);

	switch (event->type)
	{
		case MIDI_FILE_EVENT_TYPE_SYSEX:
		{
			free(event->u.sysex.data_buffer);
			break;
		}
		case MIDI_FILE_EVENT_TYPE_META:
		{
			free(event->u.meta.data_buffer);
			break;
		}
		default: ;
	}

	free(event);
	return 0;
}

MidiFileTrack_t MidiFileEvent_getTrack(MidiFileEvent_t event)
{
	if (event == NULL) return NULL;
	return event->track;
}

MidiFileEvent_t MidiFileEvent_getPreviousEvent(MidiFileEvent_t event)
{
	return MidiFileEvent_getPreviousEventInTrack(event);
}

MidiFileEvent_t MidiFileEvent_getNextEvent(MidiFileEvent_t event)
{
	return MidiFileEvent_getNextEventInTrack(event);
}

MidiFileEvent_t MidiFileEvent_getPreviousEventInTrack(MidiFileEvent_t event)
{
	if (event == NULL) return NULL;
	return event->previous_event_in_track;
}

MidiFileEvent_t MidiFileEvent_getNextEventInTrack(MidiFileEvent_t event)
{
	if (event == NULL) return NULL;
	return event->next_event_in_track;
}

MidiFileEvent_t MidiFileEvent_getPreviousEventInFile(MidiFileEvent_t event)
{
	if (event == NULL) return NULL;
	return event->previous_event_in_file;
}

MidiFileEvent_t MidiFileEvent_getNextEventInFile(MidiFileEvent_t event)
{
	if (event == NULL) return NULL;
	return event->next_event_in_file;
}

int32_t MidiFileEvent_getTick(MidiFileEvent_t event)
{
	if (event == NULL) return -1;
	return event->tick;
}

int MidiFileEvent_setTick(MidiFileEvent_t event, int32_t tick)
{
	if (event == NULL) return -1;
	remove_event(event);
	event->tick = tick;
	add_event(event);
	return 0;
}

MidiFileEventType_t MidiFileEvent_getType(MidiFileEvent_t event)
{
	if (event == NULL) return MIDI_FILE_EVENT_TYPE_INVALID;
	return event->type;
}

int MidiFileEvent_isNoteStartEvent(MidiFileEvent_t event)
{
	return ((MidiFileEvent_getType(event) == MIDI_FILE_EVENT_TYPE_NOTE_ON) && (MidiFileNoteOnEvent_getVelocity(event) > 0));
}

int MidiFileEvent_isNoteEndEvent(MidiFileEvent_t event)
{
	return ((MidiFileEvent_getType(event) == MIDI_FILE_EVENT_TYPE_NOTE_OFF) || ((MidiFileEvent_getType(event) == MIDI_FILE_EVENT_TYPE_NOTE_ON) && (MidiFileNoteOnEvent_getVelocity(event) == 0)));
}

int MidiFileEvent_isTempoEvent(MidiFileEvent_t event)
{
	return ((MidiFileEvent_getType(event) == MIDI_FILE_EVENT_TYPE_META) && (MidiFileMetaEvent_getNumber(event) == 0x51));
}

int MidiFileEvent_isVoiceEvent(MidiFileEvent_t event)
{
	switch (event->type)
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		case MIDI_FILE_EVENT_TYPE_KEY_PRESSURE:
		case MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE:
		case MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE:
		case MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE:
		case MIDI_FILE_EVENT_TYPE_PITCH_WHEEL:
		{
			return 1;
		}
		default:
		{
			return 0;
		}
	}
}

int MidiFileNoteOffEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_OFF)) return -1;
	return event->u.note_off.channel;
}

int MidiFileNoteOffEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_OFF)) return -1;
	event->u.note_off.channel = channel;
	return 0;
}

int MidiFileNoteOffEvent_getNote(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_OFF)) return -1;
	return event->u.note_off.note;
}

int MidiFileNoteOffEvent_setNote(MidiFileEvent_t event, int note)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_OFF)) return -1;
	event->u.note_off.note = note;
	return 0;
}

int MidiFileNoteOffEvent_getVelocity(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_OFF)) return -1;
	return event->u.note_off.velocity;
}

int MidiFileNoteOffEvent_setVelocity(MidiFileEvent_t event, int velocity)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_OFF)) return -1;
	event->u.note_off.velocity = velocity;
	return 0;
}

int MidiFileNoteOnEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_ON)) return -1;
	return event->u.note_on.channel;
}

int MidiFileNoteOnEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_ON)) return -1;
	event->u.note_on.channel = channel;
	return 0;
}

int MidiFileNoteOnEvent_getNote(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_ON)) return -1;
	return event->u.note_on.note;
}

int MidiFileNoteOnEvent_setNote(MidiFileEvent_t event, int note)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_ON)) return -1;
	event->u.note_on.note = note;
	return 0;
}

int MidiFileNoteOnEvent_getVelocity(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_ON)) return -1;
	return event->u.note_on.velocity;
}

int MidiFileNoteOnEvent_setVelocity(MidiFileEvent_t event, int velocity)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_NOTE_ON)) return -1;
	event->u.note_on.velocity = velocity;
	return 0;
}

int MidiFileKeyPressureEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_KEY_PRESSURE)) return -1;
	return event->u.key_pressure.channel;
}

int MidiFileKeyPressureEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_KEY_PRESSURE)) return -1;
	event->u.key_pressure.channel = channel;
	return 0;
}

int MidiFileKeyPressureEvent_getNote(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_KEY_PRESSURE)) return -1;
	return event->u.key_pressure.note;
}

int MidiFileKeyPressureEvent_setNote(MidiFileEvent_t event, int note)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_KEY_PRESSURE)) return -1;
	event->u.key_pressure.note = note;
	return 0;
}

int MidiFileKeyPressureEvent_getAmount(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_KEY_PRESSURE)) return -1;
	return event->u.key_pressure.amount;
}

int MidiFileKeyPressureEvent_setAmount(MidiFileEvent_t event, int amount)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_KEY_PRESSURE)) return -1;
	event->u.key_pressure.amount = amount;
	return 0;
}

int MidiFileControlChangeEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE)) return -1;
	return event->u.control_change.channel;
}

int MidiFileControlChangeEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE)) return -1;
	event->u.control_change.channel = channel;
	return 0;
}

int MidiFileControlChangeEvent_getNumber(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE)) return -1;
	return event->u.control_change.number;
}

int MidiFileControlChangeEvent_setNumber(MidiFileEvent_t event, int number)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE)) return -1;
	event->u.control_change.number = number;
	return 0;
}

int MidiFileControlChangeEvent_getValue(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE)) return -1;
	return event->u.control_change.value;
}

int MidiFileControlChangeEvent_setValue(MidiFileEvent_t event, int value)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE)) return -1;
	event->u.control_change.value = value;
	return 0;
}

int MidiFileProgramChangeEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE)) return -1;
	return event->u.program_change.channel;
}

int MidiFileProgramChangeEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE)) return -1;
	event->u.program_change.channel = channel;
	return 0;
}

int MidiFileProgramChangeEvent_getNumber(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE)) return -1;
	return event->u.program_change.number;
}

int MidiFileProgramChangeEvent_setNumber(MidiFileEvent_t event, int number)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE)) return -1;
	event->u.program_change.number = number;
	return 0;
}

int MidiFileChannelPressureEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE)) return -1;
	return event->u.channel_pressure.channel;
}

int MidiFileChannelPressureEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE)) return -1;
	event->u.channel_pressure.channel = channel;
	return 0;
}

int MidiFileChannelPressureEvent_getAmount(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE)) return -1;
	return event->u.channel_pressure.amount;
}

int MidiFileChannelPressureEvent_setAmount(MidiFileEvent_t event, int amount)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE)) return -1;
	event->u.channel_pressure.amount = amount;
	return 0;
}

int MidiFilePitchWheelEvent_getChannel(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PITCH_WHEEL)) return -1;
	return event->u.pitch_wheel.channel;
}

int MidiFilePitchWheelEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PITCH_WHEEL)) return -1;
	event->u.pitch_wheel.channel = channel;
	return 0;
}

int MidiFilePitchWheelEvent_getValue(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PITCH_WHEEL)) return -1;
	return event->u.pitch_wheel.value;
}

int MidiFilePitchWheelEvent_setValue(MidiFileEvent_t event, int value)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_PITCH_WHEEL)) return -1;
	event->u.pitch_wheel.value = value;
	return 0;
}

int MidiFileSysexEvent_getDataLength(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_SYSEX)) return -1;
	return event->u.sysex.data_length;
}

unsigned char *MidiFileSysexEvent_getData(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_SYSEX)) return NULL;
	return event->u.sysex.data_buffer;
}

int MidiFileSysexEvent_setData(MidiFileEvent_t event, int data_length, unsigned char *data_buffer)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_SYSEX) || (data_length < 1) || (data_buffer == NULL)) return -1;
	free(event->u.sysex.data_buffer);
	event->u.sysex.data_length = data_length;
	event->u.sysex.data_buffer = malloc(data_length);
	memcpy(event->u.sysex.data_buffer, data_buffer, data_length);
	return 0;
}

int MidiFileMetaEvent_getNumber(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_META)) return -1;
	return event->u.meta.number;
}

int MidiFileMetaEvent_setNumber(MidiFileEvent_t event, int number)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_META)) return -1;
	event->u.meta.number = number;
	return 0;
}

int MidiFileMetaEvent_getDataLength(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_META)) return -1;
	return event->u.meta.data_length;
}

unsigned char *MidiFileMetaEvent_getData(MidiFileEvent_t event)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_META)) return NULL;
	return event->u.meta.data_buffer;
}

int MidiFileMetaEvent_setData(MidiFileEvent_t event, int data_length, unsigned char *data_buffer)
{
	if ((event == NULL) || (event->type != MIDI_FILE_EVENT_TYPE_META) || (data_length < 1) || (data_buffer == NULL)) return -1;
	free(event->u.meta.data_buffer);
	event->u.meta.data_length = data_length;
	event->u.meta.data_buffer = malloc(data_length);
	memcpy(event->u.meta.data_buffer, data_buffer, data_length);
	return 0;
}

int MidiFileNoteStartEvent_getChannel(MidiFileEvent_t event)
{
	if (! MidiFileEvent_isNoteStartEvent(event)) return -1;
	return MidiFileNoteOnEvent_getChannel(event);
}

int MidiFileNoteStartEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if (! MidiFileEvent_isNoteStartEvent(event)) return -1;
	return MidiFileNoteOnEvent_setChannel(event, channel);
}

int MidiFileNoteStartEvent_getNote(MidiFileEvent_t event)
{
	if (! MidiFileEvent_isNoteStartEvent(event)) return -1;
	return MidiFileNoteOnEvent_getNote(event);
}

int MidiFileNoteStartEvent_setNote(MidiFileEvent_t event, int note)
{
	if (! MidiFileEvent_isNoteStartEvent(event)) return -1;
	return MidiFileNoteOnEvent_setNote(event, note);
}

int MidiFileNoteStartEvent_getVelocity(MidiFileEvent_t event)
{
	if (! MidiFileEvent_isNoteStartEvent(event)) return -1;
	return MidiFileNoteOnEvent_getVelocity(event);
}

int MidiFileNoteStartEvent_setVelocity(MidiFileEvent_t event, int velocity)
{
	if (! MidiFileEvent_isNoteStartEvent(event)) return -1;
	return MidiFileNoteOnEvent_setVelocity(event, velocity);
}

MidiFileEvent_t MidiFileNoteStartEvent_getNoteEndEvent(MidiFileEvent_t event)
{
	MidiFileEvent_t subsequent_event;

	if (! MidiFileEvent_isNoteStartEvent(event)) return NULL;

	for (subsequent_event = MidiFileEvent_getNextEventInTrack(event); subsequent_event != NULL; subsequent_event = MidiFileEvent_getNextEventInTrack(subsequent_event))
	{
		if (MidiFileEvent_isNoteEndEvent(subsequent_event) && (MidiFileNoteEndEvent_getChannel(subsequent_event) == MidiFileNoteStartEvent_getChannel(event)) && (MidiFileNoteEndEvent_getNote(subsequent_event) == MidiFileNoteStartEvent_getNote(event)))
		{
			return subsequent_event;
		}
	}

	return NULL;
}

int MidiFileNoteEndEvent_getChannel(MidiFileEvent_t event)
{
	if (! MidiFileEvent_isNoteEndEvent(event)) return -1;

	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			return MidiFileNoteOnEvent_getChannel(event);
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			return MidiFileNoteOffEvent_getChannel(event);
		}
		default:
		{
			return -1;
		}
	}
}

int MidiFileNoteEndEvent_setChannel(MidiFileEvent_t event, int channel)
{
	if (! MidiFileEvent_isNoteEndEvent(event)) return -1;

	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			return MidiFileNoteOnEvent_setChannel(event, channel);
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			return MidiFileNoteOffEvent_setChannel(event, channel);
		}
		default:
		{
			return -1;
		}
	}
}

int MidiFileNoteEndEvent_getNote(MidiFileEvent_t event)
{
	if (! MidiFileEvent_isNoteEndEvent(event)) return -1;

	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			return MidiFileNoteOnEvent_getNote(event);
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			return MidiFileNoteOffEvent_getNote(event);
		}
		default:
		{
			return -1;
		}
	}
}

int MidiFileNoteEndEvent_setNote(MidiFileEvent_t event, int note)
{
	if (! MidiFileEvent_isNoteEndEvent(event)) return -1;

	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			return MidiFileNoteOnEvent_setNote(event, note);
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			return MidiFileNoteOffEvent_setNote(event, note);
		}
		default:
		{
			return -1;
		}
	}
}

int MidiFileNoteEndEvent_getVelocity(MidiFileEvent_t event)
{
	if (! MidiFileEvent_isNoteEndEvent(event)) return -1;

	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			return MidiFileNoteOffEvent_getVelocity(event);
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			return 0;
		}
		default:
		{
			return -1;
		}
	}
}

int MidiFileNoteEndEvent_setVelocity(MidiFileEvent_t event, int velocity)
{
	if (! MidiFileEvent_isNoteEndEvent(event)) return -1;

	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			return MidiFileNoteOffEvent_setVelocity(event, velocity);
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			MidiFileTrack_createNoteOffEvent(MidiFileEvent_getTrack(event), MidiFileEvent_getTick(event), MidiFileNoteOnEvent_getChannel(event), MidiFileNoteOnEvent_getNote(event), velocity);
			MidiFileEvent_delete(event);
			return 0;
		}
		default:
		{
			return -1;
		}
	}
}

MidiFileEvent_t MidiFileNoteEndEvent_getNoteStartEvent(MidiFileEvent_t event)
{
	MidiFileEvent_t preceding_event;

	if (! MidiFileEvent_isNoteEndEvent(event)) return NULL;

	for (preceding_event = MidiFileEvent_getPreviousEventInTrack(event); preceding_event != NULL; preceding_event = MidiFileEvent_getPreviousEventInTrack(preceding_event))
	{
		if (MidiFileEvent_isNoteStartEvent(preceding_event) && (MidiFileNoteStartEvent_getChannel(preceding_event) == MidiFileNoteEndEvent_getChannel(event)) && (MidiFileNoteStartEvent_getNote(preceding_event) == MidiFileNoteEndEvent_getNote(event)))
		{
			return preceding_event;
		}
	}

	return NULL;
}

float MidiFileTempoEvent_getTempo(MidiFileEvent_t event)
{
	unsigned char *buffer;
	int32_t midi_tempo;

	if (! MidiFileEvent_isTempoEvent(event)) return -1;

	buffer = MidiFileMetaEvent_getData(event);
	midi_tempo = (buffer[0] << 16) | (buffer[1] << 8) | buffer[2];
	return 60000000.0 / midi_tempo;
}

int MidiFileTempoEvent_setTempo(MidiFileEvent_t event, float tempo)
{
	int32_t midi_tempo;
	unsigned char buffer[3];

	if (! MidiFileEvent_isTempoEvent(event)) return -1;

	midi_tempo = 60000000 / tempo;
	buffer[0] = (midi_tempo >> 16) & 0xFF;
	buffer[1] = (midi_tempo >> 8) & 0xFF;
	buffer[2] = midi_tempo & 0xFF;
	return MidiFileMetaEvent_setData(event, 3, buffer);
}

uint32_t MidiFileVoiceEvent_getData(MidiFileEvent_t event)
{
	switch (MidiFileEvent_getType(event))
	{
		case MIDI_FILE_EVENT_TYPE_NOTE_OFF:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0x80 | MidiFileNoteOffEvent_getChannel(event);
			u.data_as_bytes[1] = MidiFileNoteOffEvent_getNote(event);
			u.data_as_bytes[2] = MidiFileNoteOffEvent_getVelocity(event);
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		case MIDI_FILE_EVENT_TYPE_NOTE_ON:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0x90 | MidiFileNoteOnEvent_getChannel(event);
			u.data_as_bytes[1] = MidiFileNoteOnEvent_getNote(event);
			u.data_as_bytes[2] = MidiFileNoteOnEvent_getVelocity(event);
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		case MIDI_FILE_EVENT_TYPE_KEY_PRESSURE:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0xA0 | MidiFileKeyPressureEvent_getChannel(event);
			u.data_as_bytes[1] = MidiFileKeyPressureEvent_getNote(event);
			u.data_as_bytes[2] = MidiFileKeyPressureEvent_getAmount(event);
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		case MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0xB0 | MidiFileControlChangeEvent_getChannel(event);
			u.data_as_bytes[1] = MidiFileControlChangeEvent_getNumber(event);
			u.data_as_bytes[2] = MidiFileControlChangeEvent_getValue(event);
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		case MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0xC0 | MidiFileProgramChangeEvent_getChannel(event);
			u.data_as_bytes[1] = MidiFileProgramChangeEvent_getNumber(event);
			u.data_as_bytes[2] = 0;
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		case MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0xD0 | MidiFileChannelPressureEvent_getChannel(event);
			u.data_as_bytes[1] = MidiFileChannelPressureEvent_getAmount(event);
			u.data_as_bytes[2] = 0;
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		case MIDI_FILE_EVENT_TYPE_PITCH_WHEEL:
		{
			union
			{
				unsigned char data_as_bytes[4];
				uint32_t data_as_uint32;
			}
			u;

			u.data_as_bytes[0] = 0xE0 | MidiFilePitchWheelEvent_getChannel(event);
			// fixed reversed byte order -- ag
			u.data_as_bytes[1] = MidiFilePitchWheelEvent_getValue(event) & 0x7F;
			u.data_as_bytes[2] = (MidiFilePitchWheelEvent_getValue(event) >> 7) & 0x7F;
			u.data_as_bytes[3] = 0;
			return u.data_as_uint32;
		}
		default:
		{
			return 0;
		}
	}
}

int MidiFileVoiceEvent_setData(MidiFileEvent_t event, uint32_t data)
{
	union
	{
		uint32_t data_as_uint32;
		unsigned char data_as_bytes[4];
	}
	u;

	if (event == NULL) return -1;

	u.data_as_uint32 = data;

	switch (u.data_as_bytes[0] & 0xF0)
	{
		case 0x80:
		{
			event->type = MIDI_FILE_EVENT_TYPE_NOTE_OFF;
			event->u.note_off.channel = u.data_as_bytes[0] & 0x0F;
			event->u.note_off.note = u.data_as_bytes[1];
			event->u.note_off.velocity = u.data_as_bytes[2];
			return 0;
		}
		case 0x90:
		{
			event->type = MIDI_FILE_EVENT_TYPE_NOTE_ON;
			event->u.note_on.channel = u.data_as_bytes[0] & 0x0F;
			event->u.note_on.note = u.data_as_bytes[1];
			event->u.note_on.velocity = u.data_as_bytes[2];
			return 0;
		}
		case 0xA0:
		{
			event->type = MIDI_FILE_EVENT_TYPE_KEY_PRESSURE;
			event->u.key_pressure.channel = u.data_as_bytes[0] & 0x0F;
			event->u.key_pressure.note = u.data_as_bytes[1];
			event->u.key_pressure.amount = u.data_as_bytes[2];
			return 0;
		}
		case 0xB0:
		{
			event->type = MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE;
			event->u.control_change.channel = u.data_as_bytes[0] & 0x0F;
			event->u.control_change.number = u.data_as_bytes[1];
			event->u.control_change.value = u.data_as_bytes[2];
			return 0;
		}
		case 0xC0:
		{
			event->type = MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE;
			event->u.program_change.channel = u.data_as_bytes[0] & 0x0F;
			event->u.program_change.number = u.data_as_bytes[1];
			return 0;
		}
		case 0xD0:
		{
			event->type = MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE;
			event->u.channel_pressure.channel = u.data_as_bytes[0] & 0x0F;
			event->u.channel_pressure.amount = u.data_as_bytes[1];
			return 0;
		}
		case 0xE0:
		{
			event->type = MIDI_FILE_EVENT_TYPE_PITCH_WHEEL;
			event->u.pitch_wheel.channel = u.data_as_bytes[0] & 0x0F;
			// fixed reversed byte order -- ag
			event->u.pitch_wheel.value = u.data_as_bytes[1] | (u.data_as_bytes[2] << 7);
			return 0;
		}
		default:
		{
			return -1;
		}
	}
}

