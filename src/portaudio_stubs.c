/*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-portaudio.
 *
 * ocaml-portaudio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-portaudio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-portaudio; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by INRIA, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 */



#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <string.h>
#include <assert.h>

#include <portaudio.h>
#include<stdio.h>

typedef struct stream__t
{
  PaStream *stream;
  int channels_in;
  int channels_out;
  int sample_format_in;
  int sample_format_out;
  char cb[512];
} stream_t;

#define Stream_t_val(v) (*((stream_t**)Data_custom_val(v)))
#define Stream_val(v) (Stream_t_val(v))->stream


/* Check for errors. */
static int cerr(int ret)
{
  if (ret >= 0)
    return ret;

  switch(ret)
  {
    case paUnanticipatedHostError:
      caml_raise_constant(*caml_named_value("portaudio_exn_unanticipated_host_error"));

    default:
      caml_raise_with_arg(*caml_named_value("portaudio_exn_error"), Val_int(ret));
  }
}

CAMLprim value ocaml_pa_get_last_host_error_info(value unit)
{
  CAMLparam0();
  const PaHostErrorInfo *info = Pa_GetLastHostErrorInfo();
  CAMLlocal1(ans);

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(info->errorCode));
  Store_field(ans, 1, caml_copy_string(info->errorText));

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_get_version(value unit)
{
  return Val_int(Pa_GetVersion());
}

CAMLprim value ocaml_pa_get_version_text(value unit)
{
  return caml_copy_string(Pa_GetVersionText());
}

CAMLprim value ocaml_pa_get_error_text(value n)
{
  return caml_copy_string(Pa_GetErrorText(Int_val(n)));
}

CAMLprim value ocaml_pa_initialize(value unit)
{
  cerr(Pa_Initialize());

  return Val_unit;
}

CAMLprim value ocaml_pa_terminate(value unit)
{
  cerr(Pa_Terminate());

  return Val_unit;
}

CAMLprim value ocaml_pa_get_host_api_count(value unit)
{
  return Val_int(cerr(Pa_GetHostApiCount()));
}

CAMLprim value ocaml_pa_get_default_host_api(value unit)
{
  return Val_int(cerr(Pa_GetDefaultHostApi()));
}

CAMLprim value ocaml_pa_get_default_input_device(value unit)
{
  return Val_int(cerr(Pa_GetDefaultInputDevice()));
}

CAMLprim value ocaml_pa_get_default_output_device(value unit)
{
  return Val_int(cerr(Pa_GetDefaultOutputDevice()));
}

CAMLprim value ocaml_pa_get_device_count(value unit)
{
  return Val_int(cerr(Pa_GetDeviceCount()));
}

static int get_ba_type(int fmt)
{
    if(fmt & paFloat32)
        return CAML_BA_FLOAT32;

    return 0;
}

value alloc_ba_input(const void *data, unsigned long frames, stream_t *st)
{
    int type = get_ba_type(st->sample_format_in);

    if(st->channels_in > 0)
    {
        if(st->sample_format_in & paNonInterleaved)
        {
            return caml_ba_alloc_dims(CAML_BA_FLOAT32 | CAML_BA_C_LAYOUT, 2, (void*)data, st->channels_in, frames, NULL);
        }
        return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 1, (void*)data, st->channels_in*frames, NULL);
    }
    return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 0, NULL, NULL);
}

value alloc_ba_output(void *data, unsigned long frames, stream_t *st)
{
    int type = get_ba_type(st->sample_format_out);

    if(st->channels_out > 0)
    {
        if(st->sample_format_out & paNonInterleaved)
        {
            return caml_ba_alloc_dims(CAML_BA_FLOAT32 | CAML_BA_C_LAYOUT, 2, data, st->channels_out, frames, NULL);
        }
        return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 1, data, st->channels_out*frames, NULL);
    }
    return caml_ba_alloc_dims(type | CAML_BA_C_LAYOUT, 0, NULL, NULL);
}

int pa_callback(const void *input_buffer,
                void *output_buffer,
                unsigned long frames_per_buffer,
                const PaStreamCallbackTimeInfo *time_info,
                PaStreamCallbackFlags status_flags,
                void *user_data)
{
    stream_t *st = (stream_t*)user_data;

    caml_acquire_runtime_system();
    value in, out;
    in = alloc_ba_input(input_buffer, frames_per_buffer, st);
    out = alloc_ba_output(output_buffer, frames_per_buffer, st);
    caml_callback3(*caml_named_value(st->cb), in, out, Val_int(frames_per_buffer));
    caml_release_runtime_system();
    
    return 0;
}

static const int format_cst[6] = {paInt8, paInt16, paInt24, paInt32, paFloat32};

/* TODO: non-interleaved? */
static int fmt_val(value format)
{
  return format_cst[Int_val(format)] | paNonInterleaved;
}

/* The result must be freed after use. */
static PaStreamParameters* sp_val(value vsp)
{
  PaStreamParameters *sp = malloc(sizeof(PaStreamParameters));

  sp->channelCount = Int_val(Field(vsp, 0));
  sp->device = Int_val(Field(vsp, 1));
  sp->hostApiSpecificStreamInfo = NULL;
  sp->sampleFormat = fmt_val(Field(vsp, 2));
  sp->suggestedLatency = Double_val(Field(vsp, 3));

  return sp;
}

static void finalize_stream(value s)
{
  stream_t *st = Stream_t_val(s);

  if (st->stream)
    Pa_CloseStream(st->stream);
  free(st);
}

static struct custom_operations stream_ops =
{
  "ocaml_pa_stream",
  finalize_stream,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value ocaml_pa_open_stream(value inparam, value outparam, value rate, value frames, value flags, value cb)
{
  CAMLparam5(inparam, outparam, rate, flags, cb);
  CAMLlocal1(ans);
  stream_t *st;
  PaStream *stream;
  PaStreamParameters *ip, *op;
  int ret;

  ip = sp_val(inparam);
  op = sp_val(outparam);
  /* TODO: use flags and callback */
  ret = Pa_OpenStream(&stream, ip, op, Double_val(rate), Int_val(frames), paNoFlag, NULL, NULL);
  free(ip);
  free(op);
  cerr(ret);
  ans = caml_alloc_custom(&stream_ops, sizeof(stream_t*), 1, 0);
  st = malloc(sizeof(stream_t));
  st->stream = stream;
  st->channels_in = ip->channelCount;
  st->channels_out = op->channelCount;
  st->sample_format_in = ip->sampleFormat;
  st->sample_format_out = op->sampleFormat;
  Stream_t_val(ans) = st;

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_open_stream_byte(value *argv, int argc)
{
  return ocaml_pa_open_stream(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_pa_open_default_stream(value inchans, value outchans, value fmt, value rate, value frames, value cb)
{
  CAMLparam1(cb);
  CAMLlocal1(ans);
  stream_t *st;
  PaStream *stream;
  int ret;
  int inc = Int_val(inchans);
  int outc = Int_val(outchans);
  int sample_rate = Int_val(rate);
  int num_frames = Int_val(frames);
  int format = fmt_val(fmt);
  PaStreamCallback *callb = NULL;

  st = malloc(sizeof(stream_t));
  st->channels_in = inc;
  st->channels_out = outc;
  st->sample_format_in = format;
  st->sample_format_out = format;

  if(Is_block(cb))
  {
    char *name = String_val(Field(cb, 0));
    strncpy(st->cb, name, 512);
    callb = &pa_callback;
  }

  ret = Pa_OpenDefaultStream(&stream, inc, outc, format, sample_rate, num_frames, callb, st);
  if(ret < 0)
      free(st);
  cerr(ret);

  st->stream = stream;
  ans = caml_alloc_custom(&stream_ops, sizeof(stream_t*), 1, 0);
  Stream_t_val(ans) = st;

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_open_default_stream_byte(value *argv, int argc)
{
  return ocaml_pa_open_default_stream(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_pa_start_stream(value stream)
{
  cerr(Pa_StartStream(Stream_val(stream)));

  return Val_unit;
}

CAMLprim value ocaml_pa_stop_stream(value stream)
{
  cerr(Pa_StopStream(Stream_val(stream)));

  return Val_unit;
}

CAMLprim value ocaml_pa_abort_stream(value stream)
{
  cerr(Pa_AbortStream(Stream_val(stream)));

  return Val_unit;
}

CAMLprim value ocaml_pa_close_stream(value stream)
{
  cerr(Pa_CloseStream(Stream_val(stream)));
  Stream_val(stream) = 0;

  return Val_unit;
}

void *get_buffer(int fmt, int chans, int ofs, value buf)
{
    int dim = Caml_ba_array_val(buf)->dim[1];
    /*if(fmt & paFloat32)*/
    /*{*/
        float *bufi = Caml_ba_data_val(buf);
        float **bufo = malloc(chans * sizeof(float*));
        int c;
        for(c = 0; c < chans; c++)
        {
            bufo[c] = bufi + c*dim + ofs;
        }
        return bufo;
    /*}*/
    /*else if(fmt & paInt32)*/
    /*{*/
        /*int32 *bufi = Caml_ba_data_val(buf);*/
        /*int32 **bufo = malloc(chans * sizeof(float*));*/
        /*int c;*/
        /*for(c = 0; c < chans; c++)*/
        /*{*/
            /*bufo[c] = bufi + c*dim + ofs;*/
        /*}*/
        /*return bufo;*/
    /*}*/
    /*else if(fmt & paInt24)*/
    /*{*/
        /*int32 *bufi = Caml_ba_data_val(buf);*/
        /*int32 **bufo = malloc(chans * sizeof(float*));*/
        /*int c;*/
        /*for(c = 0; c < chans; c++)*/
        /*{*/
            /*bufo[c] = bufi + c*dim + ofs;*/
        /*}*/
        /*return bufo;*/
    /*}*/
    /*else if(fmt & paInt16)*/
    /*{*/
        /*short *bufi = Caml_ba_data_val(buf);*/
        /*short **bufo = malloc(chans * sizeof(float*));*/
        /*int c;*/
        /*for(c = 0; c < chans; c++)*/
        /*{*/
            /*bufo[c] = bufi + c*dim + ofs;*/
        /*}*/
        /*return bufo;*/
    /*}*/
    /*else if(fmt & paInt8)*/
    /*{*/
        /*char *bufi = Caml_ba_data_val(buf);*/
        /*char **bufo = malloc(chans * sizeof(float*));*/
        /*int c;*/
        /*for(c = 0; c < chans; c++)*/
        /*{*/
            /*bufo[c] = bufi + c*dim + ofs;*/
        /*}*/
        /*return bufo;*/
    /*}*/
    /*else*/
        /*return NULL;*/
}

CAMLprim value ocaml_pa_sleep(value time)
{
  CAMLparam1(time);
  caml_enter_blocking_section();
  Pa_Sleep(Int_val(time));
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_write_stream(value _stream, value _buf, value _ofs, value _len)
{
  CAMLparam2(_stream, _buf);
  PaStream *stream = Stream_val(_stream);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  PaError ret;
  void *buf;
  int chans = Stream_t_val(_stream)->channels_out;

  buf = get_buffer(Stream_t_val(_stream)->sample_format_out, chans, ofs, _buf);

  caml_enter_blocking_section();
  ret = Pa_WriteStream(stream, buf, len);
  caml_leave_blocking_section();
  free(buf);
  cerr(ret);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_read_stream(value _stream, value _buf, value _ofs, value _len)
{
  CAMLparam2(_stream, _buf);
  PaStream *stream = Stream_val(_stream);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  PaError ret;
  void *buf;
  int chans = Stream_t_val(_stream)->channels_in;

  buf = get_buffer(Stream_t_val(_stream)->sample_format_in, chans, ofs, _buf);

  caml_enter_blocking_section();
  ret = Pa_ReadStream(stream, buf, len);
  caml_leave_blocking_section();

  cerr(ret);

  CAMLreturn(Val_unit);
}
