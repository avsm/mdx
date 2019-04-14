open Cmdliner

let txt t =
  let cell = Notebook_t.{
    cell_type = `Markdown;
    metadata = {
      collapsed = None;
      scrolled = None;
    };
    source = t;
    outputs = None;
    execution_count = None;
  } in
  cell

let execution_count = ref 1

let ocaml contents =
  let cell = Notebook_t.{
    cell_type = `Code;
    metadata = {
      collapsed = None;
      scrolled = None;
    };
    source = String.concat "\n" contents;
    outputs = Some [];
    execution_count = (Some !execution_count);
  } in
  incr execution_count;
  cell


let toplevel x =
  let cell = Notebook_t.{
      cell_type = `Code;
      metadata = {
        collapsed = None;
        scrolled = None;
      };
      source = String.concat "\n" x.Mdx.Toplevel.command;
      outputs = Some [];
      execution_count = (Some !execution_count);
      }
  in
  incr execution_count;
  cell


let run _setup syntax file =
  let cells = ref [] in
  Mdx.run ?syntax file ~f:(fun _file_contents items ->
    let rec collapse_text = function
      | Mdx.Text x :: Mdx.Text y :: xs ->
        collapse_text (Mdx.Text (x ^ "\n" ^ y) :: xs)
      | x::ys -> x :: collapse_text ys
      | [] -> []
    in
    List.iter (function
    | Mdx.Text "" -> ()
    | Mdx.Text x ->
      cells := (txt x) :: !cells
    | Mdx.Block {value=OCaml; contents; _} ->
      cells := (ocaml contents) :: !cells
    | Mdx.Block {value=Toplevel xs; _} ->
      let newcells = List.rev_map toplevel xs in
      cells := newcells @ !cells
    | _ -> ()
    ) (collapse_text items);
    "OK"
  );
  let notebook = Notebook_t.{
    metadata = {
      kernelspec = {
        display_name = "OCaml 4.07.1";
        language = "OCaml";
        name = "ocaml-jupyter";
      };
      language_info = {
        name = "OCaml";
        version = "4.07.1";
        codemirror_mode = Some "text/x-ocaml";
        file_extension = ".ml";
        mimetype = "text/x-ocaml";
        nbconverter_exporter = None;
        pygments_lexer = "OCaml";
      };
    };
    nbformat = 4;
    nbformat_minor = 2;
    cells = List.rev !cells;
  } in
  Printf.fprintf stdout "%s" (Notebook_j.string_of_notebook notebook);
  0



let cmd: int Term.t * Term.info =
  let doc = "Convert an mdx file to a jupyter notebook." in
  Term.(pure run $ Cli.setup $ Cli.syntax $ Cli.file ),
  Term.info "jupyter" ~doc
