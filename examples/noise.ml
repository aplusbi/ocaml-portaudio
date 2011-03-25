open Portaudio
open Bigarray

let choose_device () =
    print_endline "-1 Quit";
    let dcount = get_device_count () in
    for d = 0 to dcount - 1 do
        let dinfo = get_device_info d in
        Printf.printf "%d\t%s\n" d dinfo.d_name
    done;
    read_int ()

let choose_format () =
    let formats = [|"format_int8"; "format_int16"; "format_int24"; "format_int32"; "format_float32"|] in
    for i = 0 to (Array.length formats) - 1 do
        let s = formats.(i) in
        Printf.printf "%d\t%s\n" i s
    done;
    read_int ()

let test_array stream init randf randv = 
    print_endline "Testing arrays...";
    let buf = Array.create 256 init in
    let bbuf = [|buf; buf|] in
    for j = 0 to 100 do
        for i = 0 to 255 do
            let rand = randf randv in
            buf.(i) <- rand;
        done;
        Portaudio.write_stream stream bbuf 0 256
    done

let test_bigarray stream batype randf randv = 
    print_endline "Testing Bigarrays...";
    let ba = Genarray.create batype c_layout [|2*256|] in
    for j = 0 to 100 do
        for i = 0 to 255 do
            let rand = randf randv in
            Genarray.set ba [|2*i|] rand;
            Genarray.set ba [|2*i+1|] rand
        done;
        Portaudio.write_stream_ba stream ba 0 256
    done

let start d = function
    | 0 ->
        let outparam = Some { channels=2; device=d; sample_format=format_int8; latency=1. } in
        let stream = open_stream None outparam 11025. 256 [] in
        start_stream stream;
        test_array stream 0 Random.int 256;
        test_bigarray stream int8_signed Random.int 256;
        close_stream stream
    | 1 ->
        let outparam = Some { channels=2; device=d; sample_format=format_int16; latency=1. } in
        let stream = open_stream None outparam 11025. 256 [] in
        start_stream stream;
        test_array stream 0 Random.int 65536;
        test_bigarray stream int16_signed Random.int 65536;
        close_stream stream
    | 2 ->
        let outparam = Some { channels=2; device=d; sample_format=format_int24; latency=1. } in
        let stream = open_stream None outparam 11025. 256 [] in
        start_stream stream;
        test_array stream Int32.zero Random.int32 (Int32.of_int (4096*4096));
        test_bigarray stream int32 Random.int32 (Int32.of_int (4096*4096));
        close_stream stream
    | 3 ->
        let outparam = Some { channels=2; device=d; sample_format=format_int32; latency=1. } in
        let stream = open_stream None outparam 11025. 256 [] in
        start_stream stream;
        test_array stream Int32.zero Random.int32 Int32.max_int;
        test_bigarray stream int32 Random.int32 Int32.max_int;
        close_stream stream
    | 4 ->
        let outparam = Some { channels=2; device=d; sample_format=format_float32; latency=1. } in
        let stream = open_stream None outparam 11025. 256 [] in
        start_stream stream;
        test_array stream 0. (fun () -> 1. -. (Random.float 2.)) ();
        test_bigarray stream float32 (fun () -> 1. -. (Random.float 2.)) ();
        close_stream stream

let rec main () =
    let d = choose_device () in
    if d = -1 then exit 0;
    let fmt = choose_format () in
    start d fmt;
    main ()

let () =
    Printf.printf "Using %s.\n%!" (get_version_string ());
    Random.self_init ();
    Portaudio.init ();
    main ()
