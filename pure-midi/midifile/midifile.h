#ifndef MIDI_FILE_INCLUDED
#define MIDI_FILE_INCLUDED

/* Copyright 1998-2006 David G. Slomin, all rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   - Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

   - Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   - Neither the name of David G. Slomin, Div, Sreal, nor the names of any
     other contributors to this software may be used to endorse or promote
     products derived from this software without specific prior written
     permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE. */

/*
 * Div's Standard MIDI File API
 * Copyright 2003-2006 by David G. Slomin
 * Provided under the terms of the BSD license
 *
 * Usage notes:
 *
 * 1.  Running status is eliminated from messages at load time; it should not
 *     be used at any other time.
 *
 * 2.  MIDI files in formats 0, 1, and 2 are supported, but the caller is
 *     responsible for placing events into the appropriate tracks.  Format
 *     0 files should only use a single track.  Format 1 files use their
 *     first track as a "conductor track" for meta events like tempo and 
 *     meter changes.
 *
 * 3.  MidiFile_visitEvents() and MidiFileTrack_visitEvents() are specially
 *     designed so that you can add, delete, or change the tick of events
 *     (thereby modifying the sorting order) without upsetting the iterator.
 *
 * 4.  Any data passed into these functions is memory-managed by the caller.
 *     Any data returned from these functions is memory-managed by the API.
 *     Don't forget to call MidiFile_free().
 *
 * 5.  This API is not thread-safe.
 *
 * 6.  You can navigate through events one track at a time, or through all
 *     tracks at once in an interwoven, time-sorted manner.
 *
 * 7.  Because a note on event with a velocity of zero is functionally
 *     equivalent to a note off event, you cannot simply look at the type of 
 *     an event to see whether it signifies the start or the end of a note.
 *     To handle this problem, convenience wrappers are provided for pseudo 
 *     "note start" and "note end" events.
 *
 * 8.  Convenience functions are provided for working with tempo and
 *     absolute time in files of format 1 or 0.  Tempo events (a particular
 *     kind of meta event) are only meaningful when using the PPQ division
 *     type.
 *
 * 9.  Events other than sysex and meta are considered "voice events".  For 
 *     interaction with other APIs, it is sometimes useful to pack their 
 *     messages into 32 bit integers.
 *
 * 10. All numbers in this API are zero-based, to correspond with the actual
 *     byte values of the MIDI protocol, rather than one-based, as they are
 *     commonly displayed to the user.  Channels range from 0 to 15, notes
 *     range from 0 to 127, etc.
 */

#include <stdint.h>

#ifdef __cplusplus
extern "c"
{
#endif

typedef struct MidiFile *MidiFile_t;
typedef struct MidiFileTrack *MidiFileTrack_t;
typedef struct MidiFileEvent *MidiFileEvent_t;
typedef void (*MidiFileEventVisitorCallback_t)(MidiFileEvent_t event, void *user_data);

typedef enum
{
	MIDI_FILE_DIVISION_TYPE_INVALID = -1,
	MIDI_FILE_DIVISION_TYPE_PPQ,
	MIDI_FILE_DIVISION_TYPE_SMPTE24,
	MIDI_FILE_DIVISION_TYPE_SMPTE25,
	MIDI_FILE_DIVISION_TYPE_SMPTE30DROP,
	MIDI_FILE_DIVISION_TYPE_SMPTE30
}
MidiFileDivisionType_t;

typedef enum
{
	MIDI_FILE_EVENT_TYPE_INVALID = -1,
	MIDI_FILE_EVENT_TYPE_NOTE_OFF,
	MIDI_FILE_EVENT_TYPE_NOTE_ON,
	MIDI_FILE_EVENT_TYPE_KEY_PRESSURE,
	MIDI_FILE_EVENT_TYPE_CONTROL_CHANGE,
	MIDI_FILE_EVENT_TYPE_PROGRAM_CHANGE,
	MIDI_FILE_EVENT_TYPE_CHANNEL_PRESSURE,
	MIDI_FILE_EVENT_TYPE_PITCH_WHEEL,
	MIDI_FILE_EVENT_TYPE_SYSEX,
	MIDI_FILE_EVENT_TYPE_META
}
MidiFileEventType_t;

MidiFile_t MidiFile_load(char *filename);
int MidiFile_save(MidiFile_t midi_file, const char* filename);

MidiFile_t MidiFile_new(int file_format, MidiFileDivisionType_t division_type, int resolution);
int MidiFile_free(MidiFile_t midi_file);
int MidiFile_getFileFormat(MidiFile_t midi_file);
int MidiFile_setFileFormat(MidiFile_t midi_file, int file_format);
MidiFileDivisionType_t MidiFile_getDivisionType(MidiFile_t midi_file);
int MidiFile_setDivisionType(MidiFile_t midi_file, MidiFileDivisionType_t division_type);
int MidiFile_getResolution(MidiFile_t midi_file);
int MidiFile_setResolution(MidiFile_t midi_file, int resolution);
MidiFileTrack_t MidiFile_createTrack(MidiFile_t midi_file);
int MidiFile_getNumberOfTracks(MidiFile_t midi_file);
MidiFileTrack_t MidiFile_getTrackByNumber(MidiFile_t midi_file, int number, int create);
MidiFileTrack_t MidiFile_getFirstTrack(MidiFile_t midi_file);
MidiFileTrack_t MidiFile_getLastTrack(MidiFile_t midi_file);
MidiFileEvent_t MidiFile_getFirstEvent(MidiFile_t midi_file);
MidiFileEvent_t MidiFile_getLastEvent(MidiFile_t midi_file);
int MidiFile_visitEvents(MidiFile_t midi_file, MidiFileEventVisitorCallback_t visitor_callback, void *user_data);
float MidiFile_getTimeFromTick(MidiFile_t midi_file, int32_t tick); /* time is in seconds */
int32_t MidiFile_getTickFromTime(MidiFile_t midi_file, float time);
float MidiFile_getBeatFromTick(MidiFile_t midi_file, int32_t tick);
int32_t MidiFile_getTickFromBeat(MidiFile_t midi_file, float beat);

int MidiFileTrack_delete(MidiFileTrack_t track);
MidiFile_t MidiFileTrack_getMidiFile(MidiFileTrack_t track);
int MidiFileTrack_getNumber(MidiFileTrack_t track);
int32_t MidiFileTrack_getEndTick(MidiFileTrack_t track);
int MidiFileTrack_setEndTick(MidiFileTrack_t track, int32_t end_tick);
MidiFileTrack_t MidiFileTrack_createTrackBefore(MidiFileTrack_t track);
MidiFileTrack_t MidiFileTrack_getPreviousTrack(MidiFileTrack_t track);
MidiFileTrack_t MidiFileTrack_getNextTrack(MidiFileTrack_t track);
MidiFileEvent_t MidiFileTrack_createNoteOffEvent(MidiFileTrack_t track, int32_t tick, int channel, int note, int velocity);
MidiFileEvent_t MidiFileTrack_createNoteOnEvent(MidiFileTrack_t track, int32_t tick, int channel, int note, int velocity);
MidiFileEvent_t MidiFileTrack_createKeyPressureEvent(MidiFileTrack_t track, int32_t tick, int channel, int note, int amount);
MidiFileEvent_t MidiFileTrack_createControlChangeEvent(MidiFileTrack_t track, int32_t tick, int channel, int number, int value);
MidiFileEvent_t MidiFileTrack_createProgramChangeEvent(MidiFileTrack_t track, int32_t tick, int channel, int number);
MidiFileEvent_t MidiFileTrack_createChannelPressureEvent(MidiFileTrack_t track, int32_t tick, int channel, int amount);
MidiFileEvent_t MidiFileTrack_createPitchWheelEvent(MidiFileTrack_t track, int32_t tick, int channel, int value);
MidiFileEvent_t MidiFileTrack_createSysexEvent(MidiFileTrack_t track, int32_t tick, int data_length, unsigned char *data_buffer);
MidiFileEvent_t MidiFileTrack_createMetaEvent(MidiFileTrack_t track, int32_t tick, int number, int data_length, unsigned char *data_buffer);
MidiFileEvent_t MidiFileTrack_createNoteStartAndEndEvents(MidiFileTrack_t track, int32_t start_tick, int32_t end_tick, int channel, int note, int start_velocity, int end_velocity); /* returns the start event */
MidiFileEvent_t MidiFileTrack_createTempoEvent(MidiFileTrack_t track, int32_t tick, float tempo); /* tempo is in BPM */
MidiFileEvent_t MidiFileTrack_createVoiceEvent(MidiFileTrack_t track, int32_t tick, uint32_t data);
MidiFileEvent_t MidiFileTrack_getFirstEvent(MidiFileTrack_t track);
MidiFileEvent_t MidiFileTrack_getLastEvent(MidiFileTrack_t track);
int MidiFileTrack_visitEvents(MidiFileTrack_t track, MidiFileEventVisitorCallback_t visitor_callback, void *user_data);

int MidiFileEvent_delete(MidiFileEvent_t event);
MidiFileTrack_t MidiFileEvent_getTrack(MidiFileEvent_t event);
MidiFileEvent_t MidiFileEvent_getPreviousEvent(MidiFileEvent_t event); /* deprecated:  use MidiFileEvent_getPreviousEventInTrack() */
MidiFileEvent_t MidiFileEvent_getNextEvent(MidiFileEvent_t event); /* deprecated:  use MidiFileEvent_getNextEventInTrack() */
MidiFileEvent_t MidiFileEvent_getPreviousEventInTrack(MidiFileEvent_t event);
MidiFileEvent_t MidiFileEvent_getNextEventInTrack(MidiFileEvent_t event);
MidiFileEvent_t MidiFileEvent_getPreviousEventInFile(MidiFileEvent_t event);
MidiFileEvent_t MidiFileEvent_getNextEventInFile(MidiFileEvent_t event);
int32_t MidiFileEvent_getTick(MidiFileEvent_t event);
int MidiFileEvent_setTick(MidiFileEvent_t event, int32_t tick);
MidiFileEventType_t MidiFileEvent_getType(MidiFileEvent_t event);
int MidiFileEvent_isNoteStartEvent(MidiFileEvent_t event);
int MidiFileEvent_isNoteEndEvent(MidiFileEvent_t event);
int MidiFileEvent_isTempoEvent(MidiFileEvent_t event);
int MidiFileEvent_isVoiceEvent(MidiFileEvent_t event);

int MidiFileNoteOffEvent_getChannel(MidiFileEvent_t event);
int MidiFileNoteOffEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileNoteOffEvent_getNote(MidiFileEvent_t event);
int MidiFileNoteOffEvent_setNote(MidiFileEvent_t event, int note);
int MidiFileNoteOffEvent_getVelocity(MidiFileEvent_t event);
int MidiFileNoteOffEvent_setVelocity(MidiFileEvent_t event, int velocity);

int MidiFileNoteOnEvent_getChannel(MidiFileEvent_t event);
int MidiFileNoteOnEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileNoteOnEvent_getNote(MidiFileEvent_t event);
int MidiFileNoteOnEvent_setNote(MidiFileEvent_t event, int note);
int MidiFileNoteOnEvent_getVelocity(MidiFileEvent_t event);
int MidiFileNoteOnEvent_setVelocity(MidiFileEvent_t event, int velocity);

int MidiFileKeyPressureEvent_getChannel(MidiFileEvent_t event);
int MidiFileKeyPressureEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileKeyPressureEvent_getNote(MidiFileEvent_t event);
int MidiFileKeyPressureEvent_setNote(MidiFileEvent_t event, int note);
int MidiFileKeyPressureEvent_getAmount(MidiFileEvent_t event);
int MidiFileKeyPressureEvent_setAmount(MidiFileEvent_t event, int amount);

int MidiFileControlChangeEvent_getChannel(MidiFileEvent_t event);
int MidiFileControlChangeEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileControlChangeEvent_getNumber(MidiFileEvent_t event);
int MidiFileControlChangeEvent_setNumber(MidiFileEvent_t event, int number);
int MidiFileControlChangeEvent_getValue(MidiFileEvent_t event);
int MidiFileControlChangeEvent_setValue(MidiFileEvent_t event, int value);

int MidiFileProgramChangeEvent_getChannel(MidiFileEvent_t event);
int MidiFileProgramChangeEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileProgramChangeEvent_getNumber(MidiFileEvent_t event);
int MidiFileProgramChangeEvent_setNumber(MidiFileEvent_t event, int number);

int MidiFileChannelPressureEvent_getChannel(MidiFileEvent_t event);
int MidiFileChannelPressureEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileChannelPressureEvent_getAmount(MidiFileEvent_t event);
int MidiFileChannelPressureEvent_setAmount(MidiFileEvent_t event, int amount);

int MidiFilePitchWheelEvent_getChannel(MidiFileEvent_t event);
int MidiFilePitchWheelEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFilePitchWheelEvent_getValue(MidiFileEvent_t event);
int MidiFilePitchWheelEvent_setValue(MidiFileEvent_t event, int value);

int MidiFileSysexEvent_getDataLength(MidiFileEvent_t event);
unsigned char *MidiFileSysexEvent_getData(MidiFileEvent_t event);
int MidiFileSysexEvent_setData(MidiFileEvent_t event, int data_length, unsigned char *data_buffer);

int MidiFileMetaEvent_getNumber(MidiFileEvent_t event);
int MidiFileMetaEvent_setNumber(MidiFileEvent_t event, int number);
int MidiFileMetaEvent_getDataLength(MidiFileEvent_t event);
unsigned char *MidiFileMetaEvent_getData(MidiFileEvent_t event);
int MidiFileMetaEvent_setData(MidiFileEvent_t event, int data_length, unsigned char *data_buffer);

int MidiFileNoteStartEvent_getChannel(MidiFileEvent_t event);
int MidiFileNoteStartEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileNoteStartEvent_getNote(MidiFileEvent_t event);
int MidiFileNoteStartEvent_setNote(MidiFileEvent_t event, int note);
int MidiFileNoteStartEvent_getVelocity(MidiFileEvent_t event);
int MidiFileNoteStartEvent_setVelocity(MidiFileEvent_t event, int velocity);
MidiFileEvent_t MidiFileNoteStartEvent_getNoteEndEvent(MidiFileEvent_t event);

int MidiFileNoteEndEvent_getChannel(MidiFileEvent_t event);
int MidiFileNoteEndEvent_setChannel(MidiFileEvent_t event, int channel);
int MidiFileNoteEndEvent_getNote(MidiFileEvent_t event);
int MidiFileNoteEndEvent_setNote(MidiFileEvent_t event, int note);
int MidiFileNoteEndEvent_getVelocity(MidiFileEvent_t event);
int MidiFileNoteEndEvent_setVelocity(MidiFileEvent_t event, int velocity); /* caution:  will replace a note on event with a note off */
MidiFileEvent_t MidiFileNoteEndEvent_getNoteStartEvent(MidiFileEvent_t event);

float MidiFileTempoEvent_getTempo(MidiFileEvent_t event);
int MidiFileTempoEvent_setTempo(MidiFileEvent_t event, float tempo);

uint32_t MidiFileVoiceEvent_getData(MidiFileEvent_t event);
int MidiFileVoiceEvent_setData(MidiFileEvent_t event, uint32_t data);

#ifdef __cplusplus
}
#endif

#endif
