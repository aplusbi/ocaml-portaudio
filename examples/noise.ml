open Portaudio
open Bigarray

let load_wav fname =
    let file = open_in_bin fname in
    seek_in file 40;
    let b0 = input_byte file in
    let b1 = input_byte file in
    let b2 = input_byte file in
    let b3 = input_byte file in
    let ch_size = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
    seek_in file (48 + ch_size);
    let len = (in_channel_length file) / 2 in
    let pcm = Array1.create float32 Bigarray.c_layout len in
    for i = 0 to len-1 do
        try
            let a = input_byte file in
            let b = input_byte file in
            let c = a lor (b lsl 8) in
            let d =
                if c <= 32768 then c
                else -(65536 - c)
            in
            Array1.set pcm i ((float_of_int d) /. 32768.)
        with End_of_file -> ()
    done;
    pcm


let buflen = 44100

let buf = load_wav "LightlyRow.wav"
let curr = ref 0

let my_callback input output len =
    Array2.blit input output
    (*Array1.blit (Array1.sub buf !curr len) (Array2.slice_left output 0);*)
    (*Array1.blit (Array1.sub buf !curr len) (Array2.slice_left output 1);*)
    (*curr := !curr + len*)

let unix_print str = 
    ignore (Unix.write Unix.stdout str 0 (String.length str))

let empty_cb input output len =
    let str = (string_of_int len) ^ "\n" in
    unix_print str

let () =
    Portaudio.init ();
    Printf.printf "Using %s.\n%!" (get_version_string ());
    Callback.register "my_callback" my_callback;
    let stream = open_default_stream ~callback:"my_callback" 1 1 buflen 0 in
    start_stream stream;
    while true do
        Portaudio.sleep 200;
        ()
    done;
    Portaudio.sleep 50000
