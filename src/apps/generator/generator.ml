open Basics ;;
open Tcsargs;;
open Arg ;;
open Tcstiming;;
open Paritygame ;;
open Generators ;;

module CommandLine =
struct
  type generator = NoGenerator
                 | YesGenerator of string * (string array -> paritygame)
  let generator = ref NoGenerator

  let set_verbosity i = if i >=0 && i <= 3 then verbosity := i
  let be_quiet _ = set_verbosity 0
  let be_verbose _ = set_verbosity 2
  let be_very_verbose _ = set_verbosity 3

  let genlist = fold_generators (fun _ ident _ t -> t ^ " " ^ ident) ""
  let genargs = ref ""
  let printsolverinfo = ref false

  let speclist =  [ (["-v"], Int(set_verbosity),
                      "<level>\n     sets the verbosity level, valid arguments are 0 (quiet), 1 (default), 2 (verbose) and 3 (very verbose)");
                    (["--generator"; "-gen"], 
                     Tuple [String (fun s -> let (gen, ident) = find_generator s in generator := YesGenerator (ident, gen));
                            String (fun a -> genargs := a)],
                     "<generator> \"<args>\"\n     use generator, valid ones are" ^ genlist)]

  let header = Info.get_title "Parity Game Generator"
end ;;

open CommandLine ;;

let _ =
  Random.self_init ();
  init_message_timing ();
  SimpleArgs.parsedef speclist (fun _ -> message 1 (fun _ -> "input file")) (header ^ "Usage:
    generator --generator [family] [parameters]\n" ^
                                              "Generates parity games to STDOUT.\n");

  message 1 (fun _ -> header);
  let game = match !generator with
    NoGenerator -> (Parsers.parse_parity_game stdin) |
    YesGenerator (ident, gen) -> (
	  message 1 (fun _ -> "Generating " ^ ident ^ " with arguments '" ^ !genargs ^ "'... ");
	  let timobj = SimpleTiming.init true in
	  let game = gen (Array.of_list (Tcsstrings.StringUtils.explode !genargs ' ')) in
	  SimpleTiming.stop timobj;
	  message 1 (fun _ -> (SimpleTiming.format timobj) ^ "\n");
	  game
    )
  in

  flush stdout;
	Paritygame.print_game game;
