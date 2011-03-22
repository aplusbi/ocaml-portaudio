open Portaudio
open Bigarray

let buflen = 44100

let my_callback input output len =
    Genarray.blit input output

let () =
    Random.self_init ();
    Portaudio.init ();
    Printf.printf "Using %s.\n%!" (get_version_string ());
    Callback.register "my_callback" my_callback;
    let stream = open_default_stream ~format:Portaudio.Format_int32 0 2 buflen 256 in
    start_stream stream;
    let buf = Array.create 256 Int32.zero in
    let ba = Genarray.create int32 c_layout [|2; 256|] in
    let count = get_host_api_count () in
    for i = 0 to count - 1 do
        let info = get_host_api_info i in
        let dcount = info.h_device_count in
        Printf.printf "%s\n" info.h_name;
        for j = 0 to dcount - 1 do
            let dinfo = get_device_info j in
            Printf.printf "\t%s\n" dinfo.d_name;
        done
    done;
    while true do
        for i = 0 to 255 do
            buf.(i) <- Random.int32 Int32.max_int;
            Genarray.set ba [|0; i|] (Random.int32 Int32.max_int);
            Genarray.set ba [|1; i|] (Random.int32 Int32.max_int)
        done;
        Portaudio.write_stream_ba stream ba 0 256
    done
