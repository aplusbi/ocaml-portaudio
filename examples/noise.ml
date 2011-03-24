open Portaudio
open Bigarray

let buflen = 44100

let my_callback input output len =
    Genarray.blit input output

let choose_device () =
    let did = ref 0 in
    let hcount = get_host_api_count () in
    for h = 0 to hcount - 1 do
        let hinfo = get_host_api_info h in
        print_endline hinfo.h_name;
        let dcount = hinfo.h_device_count in
        for d = 0 to dcount - 1 do
            let dinfo = get_device_info !did in
            Printf.printf "\t%d %s\n" !did dinfo.d_name;
            incr did
        done
    done;
    read_int ()

let () =
    Random.self_init ();
    Portaudio.init ();
    Printf.printf "Using %s.\n%!" (get_version_string ());
    Callback.register "my_callback" my_callback;
    let d = choose_device () in
    let outparam = Some { channels=2; device=d; sample_format=Format_int32; latency=1. } in
    let stream = open_stream None outparam 44100. 256 [] in
    start_stream stream;
    let buf = Array.create 256 Int32.zero in
    let ba = Genarray.create int32 c_layout [|2; 256|] in
    while true do
        for i = 0 to 255 do
            buf.(i) <- Random.int32 Int32.max_int;
            Genarray.set ba [|0; i|] (Random.int32 Int32.max_int);
            Genarray.set ba [|1; i|] (Random.int32 Int32.max_int)
        done;
        Portaudio.write_stream_ba stream ba 0 256
    done
