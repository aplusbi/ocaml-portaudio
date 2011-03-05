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
    let pcm = Genarray.create float32 Bigarray.c_layout [|len|] in
    for i = 0 to len-1 do
        try
            let a = input_byte file in
            let b = input_byte file in
            let c = a lor (b lsl 8) in
            let d =
                if c <= 32768 then c
                else -(65536 - c)
            in
            Genarray.set pcm [|i|] ((float_of_int d) /. 32768.)
        with End_of_file -> ()
    done;
    pcm

let mem_map_wav fname = 
    let file = Unix.openfile fname [Unix.O_RDONLY] 0 in
    Genarray.map_file file ~pos:(Int64.of_int 138) int16_signed Bigarray.c_layout false [|1725592|] 

let buflen = 44100

let buf = mem_map_wav "LightlyRow.wav"
let curr = ref 0

let my_callback input output len =
    Genarray.blit (Genarray.sub_left buf !curr len) output;
    curr := !curr + len

let () =
    Portaudio.init ();
    Printf.printf "Using %s.\n%!" (get_version_string ());
    Callback.register "my_callback" my_callback;
    let stream = open_default_stream ~format:Portaudio.Format_int16 ~callback:"my_callback" 1 1 buflen 0 in
    (*let stream = open_default_stream ~format:Portaudio.Format_int16 1 1 buflen 256 in*)
    start_stream stream;
    let len = (Genarray.dims buf).(0) / 256 in
    Portaudio.sleep 2000;
    Portaudio.stop_stream stream;
    Printf.printf "Done\n";
    (*for i = 0 to len do*)
        (*Portaudio.write_stream stream buf (i*256) 256*)
    (*done;*)
    ()
