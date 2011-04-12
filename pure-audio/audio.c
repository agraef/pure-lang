
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <portaudio.h>
#include <pure/runtime.h>

/* The timing infos of some versions of PortAudio v19 seem to be broken, hence
   we do the necessary bookkeeping ourselves. This can be commented out if you
   know that PortAudio's own timing facilities work ok. */
#define TIMER_KLUDGE

/* Make sure that this is defined if you have sigprocmask et al. It makes sure
   that the PortAudio callback thread isn't interrupted by some signals. This
   should be available on all modern Unixes. */
#ifndef _WIN32
#define HAVE_POSIX_SIGNALS
#endif

pure_expr *audio_driver_info(int api)
{
  const PaHostApiInfo *info = Pa_GetHostApiInfo(api);
  if (info) {
    size_t i, n = info->deviceCount;
    pure_expr *devs, **xv;
    if (n == 0)
      devs = pure_listl(0);
    else {
      if (!(xv = malloc(n*sizeof(pure_expr*))))	return 0;
      for (i = 0; i < n; i++)
	xv[i] = pure_int(Pa_HostApiDeviceIndexToDeviceIndex(api, i));
      devs = pure_listv(n, xv);
      free(xv);
    }
    return pure_tuplel(5, pure_cstring_dup(info->name),
		       pure_int(info->type), devs,
		       pure_int(info->defaultInputDevice),
		       pure_int(info->defaultOutputDevice));
  } else
    return 0;
}

pure_expr *audio_device_info(int dev)
{
  const PaDeviceInfo *info = Pa_GetDeviceInfo(dev);
  if (info)
    return pure_tuplel(5, pure_cstring_dup(info->name),
		       pure_int(info->hostApi),
		       pure_int(info->maxInputChannels),
		       pure_int(info->maxOutputChannels),
		       pure_double(info->defaultSampleRate));
  else
    return 0;
}

/*
 * ringbuffer.c
 * Ring Buffer utility from PortAudio.
 *
 * Author: Phil Burk, http://www.softsynth.com
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.audiomulch.com/portaudio/
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

typedef struct
{
  long   bufferSize; /* Number of bytes in FIFO. Power of 2. Set by
			RingBuffer_Init. */
  /* These are declared volatile because they are written by a different
     thread than the reader. */
  volatile long   writeIndex; /* Index of next writable byte. Set by
				 RingBuffer_AdvanceWriteIndex. */
  volatile long   readIndex;  /* Index of next readable byte. Set by
				 RingBuffer_AdvanceReadIndex. */
  long   bigMask;    /* Used for wrapping indices with extra bit to
			distinguish full/empty. */
  long   smallMask;  /* Used for fitting indices to buffer. */
  char *buffer;
} MyRingBuffer;

/***************************************************************************
 * Initialize FIFO.
 * numBytes must be power of 2, returns -1 if not.
 */

static void MyRingBuffer_Flush( MyRingBuffer *rbuf );

static long
MyRingBuffer_Init( MyRingBuffer *rbuf, long numBytes, void *dataPtr )
{
  if( ((numBytes-1) & numBytes) != 0) return -1; /* Not Power of two. */
  rbuf->bufferSize = numBytes;
  rbuf->buffer = (char *)dataPtr;
  MyRingBuffer_Flush( rbuf );
  rbuf->bigMask = (numBytes*2)-1;
  rbuf->smallMask = (numBytes)-1;
  return 0;
}

/***************************************************************************
** Return number of bytes available for reading. */

static inline long MyRingBuffer_GetReadAvailable( MyRingBuffer *rbuf )
{
  return ( (rbuf->writeIndex - rbuf->readIndex) & rbuf->bigMask );
}

/***************************************************************************
** Return number of bytes available for writing. */

static inline long MyRingBuffer_GetWriteAvailable( MyRingBuffer *rbuf )
{
  return ( rbuf->bufferSize - MyRingBuffer_GetReadAvailable(rbuf));
}

/***************************************************************************
** Clear buffer. Should only be called when buffer is NOT being read. */

static void MyRingBuffer_Flush( MyRingBuffer *rbuf )
{
  rbuf->writeIndex = rbuf->readIndex = 0;
}

/***************************************************************************
** Get address of region(s) to which we can write data.
** If the region is contiguous, size2 will be zero.
** If non-contiguous, size2 will be the size of second region.
** Returns room available to be written or numBytes, whichever is smaller.
*/

static inline long
MyRingBuffer_GetWriteRegions( MyRingBuffer *rbuf, long numBytes,
			      void **dataPtr1, long *sizePtr1,
			      void **dataPtr2, long *sizePtr2 )
{
  long   index;
  long   available = MyRingBuffer_GetWriteAvailable( rbuf );
  if( numBytes > available ) numBytes = available;
  /* Check to see if write is not contiguous. */
  index = rbuf->writeIndex & rbuf->smallMask;
  if( (index + numBytes) > rbuf->bufferSize ) {
    /* Write data in two blocks that wrap the buffer. */
    long   firstHalf = rbuf->bufferSize - index;
    *dataPtr1 = &rbuf->buffer[index];
    *sizePtr1 = firstHalf;
    *dataPtr2 = &rbuf->buffer[0];
    *sizePtr2 = numBytes - firstHalf;
  } else {
    *dataPtr1 = &rbuf->buffer[index];
    *sizePtr1 = numBytes;
    *dataPtr2 = NULL;
    *sizePtr2 = 0;
  }
  return numBytes;
}

/***************************************************************************
*/

static inline long
MyRingBuffer_AdvanceWriteIndex( MyRingBuffer *rbuf, long numBytes )
{
  return rbuf->writeIndex = (rbuf->writeIndex + numBytes) & rbuf->bigMask;
}

/***************************************************************************
** Get address of region(s) from which we can read data.
** If the region is contiguous, size2 will be zero.
** If non-contiguous, size2 will be the size of second region.
** Returns room available to be written or numBytes, whichever is smaller.
*/

static inline long
MyRingBuffer_GetReadRegions( MyRingBuffer *rbuf, long numBytes,
			     void **dataPtr1, long *sizePtr1,
			     void **dataPtr2, long *sizePtr2 )
{
  long   index;
  long   available = MyRingBuffer_GetReadAvailable( rbuf );
  if( numBytes > available ) numBytes = available;
  /* Check to see if read is not contiguous. */
  index = rbuf->readIndex & rbuf->smallMask;
  if( (index + numBytes) > rbuf->bufferSize ) {
    /* Write data in two blocks that wrap the buffer. */
    long firstHalf = rbuf->bufferSize - index;
    *dataPtr1 = &rbuf->buffer[index];
    *sizePtr1 = firstHalf;
    *dataPtr2 = &rbuf->buffer[0];
    *sizePtr2 = numBytes - firstHalf;
  } else {
    *dataPtr1 = &rbuf->buffer[index];
    *sizePtr1 = numBytes;
    *dataPtr2 = NULL;
    *sizePtr2 = 0;
  }
  return numBytes;
}

/***************************************************************************
*/

static inline long
MyRingBuffer_AdvanceReadIndex( MyRingBuffer *rbuf, long numBytes )
{
  return rbuf->readIndex = (rbuf->readIndex + numBytes) & rbuf->bigMask;
}

/***************************************************************************
** Return bytes written. */

static inline long
MyRingBuffer_Write( MyRingBuffer *rbuf, const void *data, long numBytes )
{
  long size1, size2, numWritten;
  void *data1, *data2;
  numWritten = MyRingBuffer_GetWriteRegions( rbuf, numBytes, &data1, &size1, &data2, &size2 );
  if( size2 > 0 ) {
    memcpy( data1, data, size1 );
    data = ((char *)data) + size1;
    memcpy( data2, data, size2 );
  } else {
    memcpy( data1, data, size1 );
  }
  MyRingBuffer_AdvanceWriteIndex( rbuf, numWritten );
  return numWritten;
}

/***************************************************************************
** Return bytes read. */

static inline long
MyRingBuffer_Read( MyRingBuffer *rbuf, void *data, long numBytes )
{
  long size1, size2, numRead;
  void *data1, *data2;
  numRead = MyRingBuffer_GetReadRegions( rbuf, numBytes, &data1, &size1,
					 &data2, &size2 );
  if( size2 > 0 ) {
    memcpy( data, data1, size1 );
    data = ((char *)data) + size1;
    memcpy( data, data2, size2 );
  } else {
    memcpy( data, data1, size1 );
  }
  MyRingBuffer_AdvanceReadIndex( rbuf, numRead );
  return numRead;
}

/***************************************************************************
*/

/* This has been ported from the Q-Audio module. We maintain our own linked
   lists of open audio streams so that we can easily perform actions on all
   streams. Our streams also do their own locking and signaling in order to
   wake up clients which are waiting to do I/O on the stream. This is used to
   implement thread-safe blocking I/O operations for Pure applications.
   (PortAudio v19 also provides its own blocking I/O but it's not implemented
   for all drivers, so we rather do our own.) */

static bool init_ok;

typedef struct _MyStream {
  PaStream *as;
  pthread_mutex_t data_mutex, in_mutex, out_mutex;
  pthread_cond_t in_cond, out_cond;
  MyRingBuffer in_buf, out_buf;
  char *in_data, *out_data;
  PaDeviceIndex in, out;
  double time, sample_rate, in_latency, out_latency;
#ifdef TIMER_KLUDGE
  double delta;
#endif
  int size;
  PaSampleFormat in_format, out_format;
  int in_channels, in_bps, in_bpf;
  int out_channels, out_bps, out_bpf;
  struct _MyStream *prev, *next;
} MyStream;

#define has_input(v) (v->in!=paNoDevice)
#define has_output(v) (v->out!=paNoDevice)

static MyStream *current = NULL;

static bool init_buf(MyRingBuffer *buf, char **data, long bufsize)
{
  if (!(*data = malloc(bufsize)))
    return false;
  memset(*data, 0, bufsize);
  if (MyRingBuffer_Init(buf, bufsize, *data)) {
    free(*data);
    return false;
  }
  return true;
}

static void fini_buf(MyRingBuffer *buf, char **data)
{
  free(*data);
  *data = NULL;
}

static unsigned long round_pow2(unsigned long n)
{
  /* round to the next power of 2 */
  long numBits = 0;
  if( ((n-1) & n) == 0) return n; /* Already a power of two. */
  while( n > 0 ) {
    n= n>>1;
    numBits++;
  }
  return (1<<numBits);
}

static bool init_stream(MyStream *v,
			PaStreamParameters *in, PaStreamParameters *out,
			long size)
{
  memset(v, 0, sizeof(MyStream));
  if (in) {
    long bufsize = round_pow2(Pa_GetSampleSize(in->sampleFormat)*
			      in->channelCount*size);
    if (!init_buf(&v->in_buf, &v->in_data, bufsize))
      return false;
  }
  if (out) {
    long bufsize = round_pow2(Pa_GetSampleSize(out->sampleFormat)*
			      out->channelCount*size);
    if (!init_buf(&v->out_buf, &v->out_data, bufsize)) {
      if (in) fini_buf(&v->in_buf, &v->in_data);
      return false;
    }
#if 0
    {
      /* fake a full write buffer */
      long bytes = MyRingBuffer_GetWriteAvailable(&v->out_buf);
      MyRingBuffer_AdvanceWriteIndex(&v->out_buf, bytes);
    }
#endif
  }
  v->time = 0.0;
#ifdef TIMER_KLUDGE
  v->delta = 0.0;
#endif
  pthread_mutex_init(&v->data_mutex, NULL);
  if (in) {
    pthread_mutex_init(&v->in_mutex, NULL);
    pthread_cond_init(&v->in_cond, NULL);
  }
  if (out) {
    pthread_mutex_init(&v->out_mutex, NULL);
    pthread_cond_init(&v->out_cond, NULL);
  }
  if (current) current->prev = v;
  v->prev = NULL;
  v->next = current;
  current = v;
  return true;
}

static void lock_stream(void *x)
{
  MyStream *v = (MyStream*)x;
  pthread_mutex_lock(&v->data_mutex);
  if (has_input(v)) pthread_mutex_lock(&v->in_mutex);
  if (has_output(v)) pthread_mutex_lock(&v->out_mutex);
}

static void unlock_stream(void *x)
{
  MyStream *v = (MyStream*)x;
  pthread_mutex_unlock(&v->data_mutex);
  if (has_input(v)) pthread_mutex_unlock(&v->in_mutex);
  if (has_output(v)) pthread_mutex_unlock(&v->out_mutex);
}

static void fini_stream(MyStream *v, bool abort)
{
  if (v->as) {
    if (abort)
      Pa_AbortStream(v->as);
    else
      Pa_StopStream(v->as);
    pthread_cleanup_push(unlock_stream, (void*)v);
    lock_stream(v);
    Pa_CloseStream(v->as);
    v->as = NULL;
    /* wake up threads waiting to read or write the stream */
    if (has_input(v)) pthread_cond_broadcast(&v->in_cond);
    if (has_output(v)) pthread_cond_broadcast(&v->out_cond);
    pthread_cleanup_pop(1);
  }
}

static void destroy_stream(MyStream *v)
{
  pthread_mutex_destroy(&v->data_mutex);
  if (has_input(v)) {
    pthread_mutex_destroy(&v->in_mutex);
    pthread_cond_destroy(&v->in_cond);
  }
  if (has_output(v)) {
    pthread_mutex_destroy(&v->out_mutex);
    pthread_cond_destroy(&v->out_cond);
  }
  if (has_input(v)) fini_buf(&v->in_buf, &v->in_data);
  if (has_output(v)) fini_buf(&v->out_buf, &v->out_data);
  if (v->prev) v->prev->next = v->next;
  if (v->next) v->next->prev = v->prev;
  if (v == current) current = v->next;
}

void start_audio(void)
{
  MyStream *v;
  for (v = current; v; v = v->next) fini_stream(v, 1);
  Pa_Terminate();
  init_ok = (Pa_Initialize() == paNoError);
}

void stop_audio(void)
{
  MyStream *v;
  for (v = current; v; v = v->next) fini_stream(v, 1);
  Pa_Terminate();
  init_ok = false;
}

/* Audio processing callback. Note that in difference to the pablio
   implementation we employ some mutex locks and condition variables here.
   This isn't nice but is needed to protect shared data and to wake up a
   client waiting for data to be read or written. */

static int audio_cb(const void *input, void *output,
		    unsigned long nframes,
		    const PaStreamCallbackTimeInfo *time_info,
		    PaStreamCallbackFlags status,
		    void *data)
{
  MyStream *v = (MyStream*)data;
  long in_bytes = v->in_bpf*nframes, out_bytes = v->out_bpf*nframes;
  /* update the current time */
  pthread_mutex_lock(&v->data_mutex);
  if (!v->as) {
    pthread_mutex_unlock(&v->data_mutex);
    return 0;
  }
#ifdef TIMER_KLUDGE
  v->time += v->delta;
  v->delta = ((double)nframes)/v->sample_rate;
#else
  v->time = time_info?time_info->currentTime:0;
#endif
  pthread_mutex_unlock(&v->data_mutex);
  /* process data */
  if (input) {
    pthread_mutex_lock(&v->in_mutex);
    if (MyRingBuffer_GetWriteAvailable(&v->in_buf) == 0)
      MyRingBuffer_AdvanceReadIndex(&v->in_buf, in_bytes);
    MyRingBuffer_Write(&v->in_buf, input, in_bytes);
    pthread_cond_signal(&v->in_cond);
    pthread_mutex_unlock(&v->in_mutex);
  }
  if (output) {
    long read;
    pthread_mutex_lock(&v->out_mutex);
    read = MyRingBuffer_Read(&v->out_buf, output, out_bytes);
    if (read < out_bytes) memset(((char*)output)+read, 0, out_bytes-read);
    pthread_cond_signal(&v->out_cond);
    pthread_mutex_unlock(&v->out_mutex);
  }
  return 0;
}

pure_expr *open_audio_stream(int *in, int *out,
			     double sr, long size, int flags)
{
  PaStreamParameters inparams, outparams, *inptr = NULL, *outptr = NULL;
  MyStream *v;
  PaError err;
  int in_bps = 0, out_bps = 0;
  const PaStreamInfo* info;
#ifdef HAVE_POSIX_SIGNALS
  sigset_t sigset, oldset;
#endif

  if (!init_ok) return 0;
  if (size <= 0) size = 512;

  /* Initialize parameters. */

  if (in && in[1]>0) {
    memset(&inparams, 0, sizeof(inparams));
    inparams.device = in[0];
    inparams.channelCount = in[1];
    inparams.sampleFormat = in[2];
    inparams.suggestedLatency = in[3] ?
      Pa_GetDeviceInfo(in[0])->defaultLowInputLatency :
      Pa_GetDeviceInfo(in[0])->defaultHighInputLatency;
    inparams.hostApiSpecificStreamInfo = 0;
    inptr = &inparams;
    in_bps = Pa_GetSampleSize(inparams.sampleFormat);
    if (in_bps <= 0) return 0;
  } else
    in = 0;

  if (out && out[1]>0) {
    memset(&outparams, 0, sizeof(outparams));
    outparams.device = out[0];
    outparams.channelCount = out[1];
    outparams.sampleFormat = out[2];
    outparams.suggestedLatency = out[3] ?
      Pa_GetDeviceInfo(out[0])->defaultLowOutputLatency :
      Pa_GetDeviceInfo(out[0])->defaultHighOutputLatency;
    outparams.hostApiSpecificStreamInfo = 0;
    outptr = &outparams;
    out_bps = Pa_GetSampleSize(outparams.sampleFormat);
    if (out_bps <= 0) return 0;
  } else
    out = 0;

  /* Initialize the Pure stream descriptor. This is passed to the callback
     data and also to the sentry on the stream object, so that we can perform
     proper cleanup when a stream is closed or gets garbage-collected. */

  if (!(v = malloc(sizeof(MyStream)))) return 0;
  if (!init_stream(v, inptr, outptr, size)) {
    free(v);
    return 0;
  }

  /* Open the PortAudio stream. */

  err = Pa_OpenStream(&v->as, inptr, outptr, sr, size, flags,
		      audio_cb, v);

  if (err != paNoError) {
    destroy_stream(v);
    free(v);
    return pure_int(err);
  }

  /* Fill in needed information about the stream. */

  info = Pa_GetStreamInfo(v->as);
  v->in = in?inparams.device:paNoDevice;
  v->out = out?outparams.device:paNoDevice;
  v->sample_rate = info?info->sampleRate:sr;
  v->size = size;
  /* Looks like these are sometimes bogus for some drivers and devices, but we
     store them anyway. */
  v->in_latency = info?info->inputLatency:0;
  v->out_latency = info?info->outputLatency:0;
  v->in_channels = in?inparams.channelCount:0;
  v->out_channels = out?outparams.channelCount:0;
  v->in_format = in?inparams.sampleFormat:0;
  v->out_format = out?outparams.sampleFormat:0;
  v->in_bps = in_bps; v->out_bps = out_bps;
  v->in_bpf = in_bps*v->in_channels;
  v->out_bpf = out_bps*v->out_channels;

  /* Start the stream. */
#ifdef HAVE_POSIX_SIGNALS
  /* temporarily block some signals to make sure that they are blocked in the
     PortAudio threads created by Pa_StartStream */
  sigemptyset(&sigset);
  sigaddset(&sigset, SIGINT);
  sigaddset(&sigset, SIGQUIT);
  sigaddset(&sigset, SIGTSTP);
  sigaddset(&sigset, SIGTERM);
  sigaddset(&sigset, SIGHUP);
  sigprocmask(SIG_BLOCK, &sigset, &oldset);
#endif
  Pa_StartStream(v->as);
#ifdef HAVE_POSIX_SIGNALS
  sigprocmask(SIG_SETMASK, &oldset, NULL);
#endif

  /* Note that we return the real PortAudio stream here, so that it can be
     passed to other (low-level) PortAudio operations. Our own stream data is
     kept in the sentry we set on the stream so that we can perform the
     necessary cleanup. */
  return pure_sentry
    (pure_app(pure_symbol(pure_sym("audio::audio_sentry")),
	      pure_pointer(v)),
     pure_pointer(v->as));
}

void audio_sentry(MyStream *v, PaStream *as)
{
  if (!v) return;
  fini_stream(v, 0);
  destroy_stream(v);
  free(v);
}

pure_expr *audio_stream_info(MyStream *v, PaStream *as)
{
  int in_info[] = {v->in, v->in_channels, v->in_format, v->in_bps},
    out_info[] = {v->out, v->out_channels, v->out_format, v->out_bps};
  return pure_tuplel
    (4, pure_double(v->sample_rate),
     pure_int(v->size),
     matrix_from_int_array(1, has_input(v)?4:0, in_info),
     matrix_from_int_array(1, has_output(v)?4:0, out_info));
}

pure_expr *audio_stream_latencies(MyStream *v, PaStream *as)
{
  return pure_tuplel
    (2, pure_double(v->in_latency), pure_double(v->out_latency));
}

double audio_stream_time(MyStream *v, PaStream *as)
{
  double time;
  pthread_mutex_lock(&v->data_mutex);
  time = v->time;
  pthread_mutex_unlock(&v->data_mutex);
  return time;
}

int audio_stream_readable(MyStream *v, PaStream *as)
{
  if (v->as && has_input(v))
    return MyRingBuffer_GetReadAvailable(&v->in_buf)/v->in_bpf;
  else
    return 0;
}

int audio_stream_writeable(MyStream *v, PaStream *as)
{
  if (v->as && has_output(v))
    return MyRingBuffer_GetWriteAvailable(&v->out_buf)/v->out_bpf;
  else
    return 0;
}

int read_audio_stream(MyStream *v, PaStream *as, void *buf, long size)
{
  if (!has_input(v)) return -1;
  if (size > 0 && buf) {
    long bytes = size*v->in_bpf, read;
    char *p = (char*)buf;
    if (!p) return -1;
    pthread_cleanup_push((void(*)(void*))pthread_mutex_unlock,
			 (void*)&v->in_mutex);
    pthread_mutex_lock(&v->in_mutex);
    while (v->as && bytes > 0) {
      while (v->as &&
	     (read = MyRingBuffer_Read(&v->in_buf, p, bytes)) == 0)
	pthread_cond_wait(&v->in_cond, &v->in_mutex);
      bytes -= read;
      p += read;
    }
    pthread_cleanup_pop(1);
    read = (size*v->in_bpf-bytes)/v->in_bpf;
    return read;
  } else if (size == 0)
    return 0;
  else
    return -1;
}

int write_audio_stream(MyStream *v, PaStream *as, void *buf, long size)
{
  if (!has_output(v)) return -1;
  if (size > 0 && buf) {
    long bytes = size*v->out_bpf, written;
    char *p = buf;
    size = bytes;
    pthread_cleanup_push((void(*)(void*))pthread_mutex_unlock,
			 (void*)&v->out_mutex);
    pthread_mutex_lock(&v->out_mutex);
    while (v->as && bytes > 0) {
      while (v->as &&
	     (written = MyRingBuffer_Write(&v->out_buf, p, bytes)) == 0)
	pthread_cond_wait(&v->out_cond, &v->out_mutex);
      bytes -= written;
      p += written;
    }
    pthread_cleanup_pop(1);
    written = (size*v->out_bpf-bytes)/v->out_bpf;
    return written;
  } else if (size == 0)
    return 0;
  else
    return -1;
}

int read_audio_stream_int(MyStream *v, PaStream *as, int *buf, long size)
{
  if (!has_input(v)) return -1;
  if (size < 0) return -1;
  if (size == 0) return 0;
  if (v->in_format == paInt32) /* immediate */
    return read_audio_stream(v, as, buf, size);
  else {
    /* Read into a temporary buffer. */
    void *p = malloc(size*v->in_bpf);
    int ret = read_audio_stream(v, as, p, size);
    long i;
    if (ret <= 0) {
      free(p); return ret;
    }
    /* Convert to int. */
    size = ret*v->in_channels;
    switch (v->in_format) {
    case paInt16: {
      short *m = (short*)p;
      for (i = 0; i < size; i++) buf[i] = m[i];
    }
    case paInt8: {
      char *m = (char*)p;
      for (i = 0; i < size; i++) buf[i] = m[i];
    }
    case paUInt8: {
      unsigned char *m = (unsigned char*)p;
      for (i = 0; i < size; i++) buf[i] = (unsigned)m[i];
    }
    case paInt24: /* TODO */
    default:
      /* Unsupported format. */
      ret = -1; break;
    }
    free(p);
    return ret;
  }
}

int read_audio_stream_double(MyStream *v, PaStream *as, double *buf, long size)
{
  if (!has_input(v)) return -1;
  if (size < 0) return -1;
  if (size == 0) return 0;
  if (v->in_format != paFloat32)
    return -1;
  else {
    /* Read into a temporary buffer. */
    float *m = malloc(size*v->in_bpf);
    int ret = read_audio_stream(v, as, m, size);
    long i;
    if (ret <= 0) {
      free(m); return ret;
    }
    /* Convert to double. */
    size = ret*v->in_channels;
    for (i = 0; i < size; i++) buf[i] = m[i];
    free(m);
    return ret;
  }
}

int write_audio_stream_int(MyStream *v, PaStream *as, int *buf, long size)
{
  if (!has_output(v)) return -1;
  if (size < 0) return -1;
  if (size == 0) return 0;
  if (v->out_format == paInt32) /* immediate */
    return write_audio_stream(v, as, buf, size);
  else {
    /* Write from a temporary buffer. */
    int ret;
    long i, n = size*v->out_channels;
    void *p = malloc(size*v->out_bpf);
    if (!p) return -1;
    /* Convert from int. */
    switch (v->out_format) {
    case paInt16: {
      short *m = (short*)p;
      for (i = 0; i < n; i++) m[i] = buf[i];
    }
    case paInt8: {
      char *m = (char*)p;
      for (i = 0; i < n; i++) m[i] = buf[i];
    }
    case paUInt8: {
      unsigned char *m = (unsigned char*)p;
      for (i = 0; i < n; i++) m[i] = (unsigned)buf[i];
    }
    case paInt24: /* TODO */
    default:
      /* Unsupported format. */
      free(p); return -1;
    }
    ret = write_audio_stream(v, as, p, size);
    free(p);
    return ret;
  }
}

int write_audio_stream_double(MyStream *v, PaStream *as, double *buf, long size)
{
  if (!has_output(v)) return -1;
  if (size < 0) return -1;
  if (size == 0) return 0;
  if (v->out_format != paFloat32)
    return -1;
  else {
    /* Write from a temporary buffer. */
    int ret;
    long i, n = size*v->out_channels;
    float *m = malloc(size*v->out_bpf);
    if (!m) return -1;
    /* Convert from double. */
    for (i = 0; i < n; i++) m[i] = buf[i];
    ret = write_audio_stream(v, as, m, size);
    free(m);
    return ret;
  }
}
