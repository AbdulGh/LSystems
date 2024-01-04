(* utils *)

let appendchar (s: string) (c: char): string = s ^ (String.make 1 c)

let strlen = String.length 
let strsub = String.sub
let map = List.map

let chars = List.init 256 Char.chr

type rule = char * string
type state = int
type tfun_t = state -> char -> state option 
type total_tfun_t = state -> char -> state

type fsm = {
    startstate: state;
    numstates: int;
    accept: state -> char option;
    tfun : tfun_t
}

type total_fsm = {
    startstate: state;
    numstates: int;
    accept: state -> char option;
    tfun : total_tfun_t
}

(*
 * Simple fsm from to accept a finite set of strings, emitting the rule lhs
 * We start with just the state 1, an empty accept and tfun
 * When we insert some rule c -> s_1,...,s_n,
 *  Start in the start state (1)
 *  Follow the fsm while we can
 *  As we are assuming some prefix free grammar atm, stop if the state is accepting
 *  If we ever leave the state machine, just add a path of states
*)
(*todo document*)
let chain_tfun (start: state) (input: string) (index: int): tfun_t =
    fun s c -> 
        if (s >= start && s < start + (strlen input) - index && c = input.[s - start + index])
        then Some (s + 1)
        else None 

let chain_fsm (startstate: state) ((c, s): rule) (firstchar: int): fsm =
    let numstates = (strlen s) - firstchar + 1 in {
        startstate = startstate;
        numstates = numstates;
        accept = (fun s2 -> if s2 = startstate + numstates - 1 then Some c else None);
        tfun = chain_tfun startstate s firstchar
    }
    
let longest_overlap (suf: string) (pref: string): int = 
    let rec longest_overlap_inner (check: int) (acc: int): int = 
        if check > min (strlen suf) (strlen pref)
            then acc
            else longest_overlap_inner (check + 1) ( (* todo check if this counts as tail recursive *)
                if strsub pref 0 check = strsub suf (strlen suf - check) check
                    then check
                    else acc
            )
    in longest_overlap_inner 1 0

exception NPE
let checked_access: 'a option -> 'a = function
    | Some thing -> thing;
    | None -> raise NPE 

(*
    returns the last state that the string goes to,
    and the index of the first character leaving the fsm if it exists, else one past the end
*)

let follow_str (mach: fsm) (str: string): state * int =
    let rec follow_str_inner (ind: int) (st: state): state * int =
        if ind = strlen str then (st, ind)
        else match mach.tfun st str.[ind] with
         | None -> (st, ind)
         | Some nextstate -> follow_str_inner (ind + 1) nextstate
    in follow_str_inner 0 mach.startstate 

(* todo use accumulator *)
(* 
   returns the string with the longest prefix matching a suffix of str,
   and the length of that overlap (>= 0)
*)
let rec failurefun_prefix (str: string) (strs: string list): (string * int) = 
    match strs with
     | [s] -> (s, longest_overlap str s);
     | h :: t ->
       let 
        current = longest_overlap str h 
        and (ns, nl) as nextbest = failurefun_prefix str t
       in
        if current >= nl then (h, current) else nextbest

(*
    gives, for each short string with last character leaving the fsm, the state reached by the longest prefix
    of a rule rhs matching a suffix of that string
*)
let failurefun (mach: fsm) (failstr: string) (strs: string list): state = 
    let (beststr, overlap) = failurefun_prefix failstr strs in fst (follow_str mach (strsub beststr 0 overlap))
    
(* step 1: just throw together a fsm *)

let rec basicfsm_inner (strs: rule list): fsm
    = match strs with
     | [w] -> chain_fsm 0 w 0;
     | ((clhc, cstr) as rule) :: t ->
        let nextfsm = basicfsm_inner t in
            let (laststate, firstchar) = follow_str nextfsm cstr in 
                if firstchar = (strlen cstr) 
                then { (*the case where we dont need to add a chain*)
                    startstate = 0;
                    numstates = nextfsm.numstates;
                    accept = (fun s -> if s = laststate then Some clhc else (nextfsm.accept s));
                    tfun = nextfsm.tfun
                } 
                else let chain = chain_fsm nextfsm.numstates rule (firstchar + 1) in {
                    startstate = 0;
                    numstates = nextfsm.numstates + (strlen cstr) - firstchar; 
                    accept = (fun s -> if s >= nextfsm.numstates then chain.accept s else nextfsm.accept s); (*todo check overlapping*)
                    tfun = (
                        fun s c -> match s, c with
                        | s2, c2 when s2 = laststate && c2 = cstr.[firstchar] -> Some chain.startstate; (* = nextfsm.numstates *)
                        | s2, c2 when s >= nextfsm.numstates -> chain.tfun s2 c2;
                        | s2, c2 -> nextfsm.tfun s2 c2
                    )
                }

let basicfsm (strs: rule list): fsm = 
    if strs = []
        then failwith "Expected some rules in basicfsm" 
        else basicfsm_inner strs

(* step 2 - update basicfsm tfun w/ failurefun *)
(* i.e., for all strings, and all their prefixes, and all the characters not equal to the next one, add the failure transition *)

let rec combine_fns (fns: ('a -> 'b -> 'c option) list): 'a -> 'b -> 'c option =
    match fns with
    | [] -> (fun _ _ -> None);
    | h :: t -> (
        fun x y -> match (h x y) with
        | Some thing -> Some thing;
        | None -> (combine_fns t) x y
    )
    
let make_fsm_add_str (trie: fsm) (strs: string list) (str: string): tfun_t = 
    let rec make_fsm_add_chrs (cstate: state) (index: int): tfun_t list =
        (fun si ci -> 
            if si == cstate
                then Some (failurefun trie (appendchar (strsub str 0 index)  ci) strs)
                else None
        ) :: if index < strlen str 
                then make_fsm_add_chrs (checked_access (trie.tfun cstate str.[index])) (index + 1)
                else []
    in combine_fns (make_fsm_add_chrs trie.startstate 0)

let make_fsm (rules: rule list): fsm =
    let rules = List.sort (* very important! makes the use of failurefun acceptable *)
        (fun (_, s1) (_, s2) -> compare (strlen s1) (strlen s2))
        rules
    in
        let trie = basicfsm rules and strs = (map snd rules) in
            let combinedfun = combine_fns (map (make_fsm_add_str trie strs) strs) in {
                startstate = 0;
                numstates = trie.numstates;
                accept = trie.accept;
                tfun = (
                    fun s c -> match trie.tfun s c with
                     | Some state -> Some state;
                     | None -> combinedfun s c
                )
            }

(*step 3: flatten the 'decision tree' in the tfun which should now be total*)
let flatten_fsm (mach: fsm): total_fsm =
    let init_row (s: state) = Array.init 256 (fun ccode -> checked_access (mach.tfun s (Char.chr ccode))) in
    let table = Array.init mach.numstates init_row in {
        startstate = mach.startstate;
        numstates = mach.numstates;
        accept = mach.accept;
        tfun = fun s c -> table.(s).(Char.code c)
    }

(* acceptance stuff *)
let fsm_accepts (mach: fsm) (input: string): char option =
    let rec fsm_accepts_inner (cstate: state) (index: int): char option =
        if index = strlen input then mach.accept cstate
        else match mach.tfun cstate input.[index] with
        | Some ns -> fsm_accepts_inner ns (index + 1);
        | None -> None
    in fsm_accepts_inner 0 0

let total_fsm_accepts (mach: total_fsm) (input: string): char option =
    let rec tfsm_accepts_inner (s: state) (index: int): char option =
        if index = strlen input
            then mach.accept s
            else tfsm_accepts_inner (mach.tfun s input.[index]) (index + 1)
    in tfsm_accepts_inner mach.startstate 0