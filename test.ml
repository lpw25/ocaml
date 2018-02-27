let init n f =
  let () = () in
  (fun () ->
  Bytes.init n f |> Bytes.unsafe_to_string)
