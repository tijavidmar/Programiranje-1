(*----------------------------------------------------------------------------*
 # 2. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 V tej domači nalogi bomo besedila stiskali s Huffmanovim kodiranjem. Vsak
 znak namesto z 8 biti zapišemo s kodo, ki je za pogoste znake krajša, za
 redke pa daljša. Kode preberemo iz dvojiškega drevesa, v katerega listih so
 znaki: koda znaka je pot od korena do njegovega lista, pri čemer korak levo
 zapišemo z bitom `O`, korak desno pa z bitom `I`. Za besedilo "abrakadabra"
 je najboljše tako drevo:

             *
            / \
           a   *
              / \
             r   *
                / \
               b   *
                  / \
                 d   k

 Znak `a` ima kodo `O`, `r` kodo `IO`, `b` kodo `IIO`, `d` kodo `IIIO` in `k`
 kodo `IIII`, zato celotno besedilo namesto 88 bitov zasede le 23. Ker so
 znaki le v listih, nobena koda ni začetek kake druge, zato lahko zaporedje
 bitov enolično preberemo nazaj.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Kodiranje z danim drevesom
[*----------------------------------------------------------------------------*)

type bit = O | I
type drevo = List of char | Vozlisce of drevo * drevo

let primer_drevesa =
  Vozlisce
    ( List 'a',
      Vozlisce (List 'r', Vozlisce (List 'b', Vozlisce (List 'd', List 'k'))) )

(*----------------------------------------------------------------------------*
 Na voljo imate funkciji, ki niz pretvorita v seznam znakov in obratno.
[*----------------------------------------------------------------------------*)

let znaki niz = List.init (String.length niz) (String.get niz)
let iz_znakov znaki = String.concat "" (List.map (String.make 1) znaki)

(*----------------------------------------------------------------------------*
 Sezname bitov bomo zaradi preglednosti zapisovali kot nize ničel in enic.
 Sestavite funkciji `v_niz_bitov : bit list -> string` in
 `iz_niza_bitov : string -> bit list`, ki pretvarjata med obema zapisoma.

 # v_niz_bitov [O; I; I; O];;
 - : string = "0110"
 # iz_niza_bitov "0110";;
 - : bit list = [O; I; I; O]
[*----------------------------------------------------------------------------*)

let rec v_niz_bitov biti =
  match biti with
  | [] -> ""
  | x :: xs ->
      (if x = O then "0" else "1") ^ v_niz_bitov xs

let rec iz_niza_bitov niz =
  match znaki niz with
  | '0' :: xs -> O :: iz_niza_bitov (iz_znakov xs)
  | '1' :: xs -> I :: iz_niza_bitov (iz_znakov xs)
  | _ -> []

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `tabela_kod : drevo -> (char * bit list) list`, ki vrne
 kode vseh znakov v drevesu v vrstnem redu listov od leve proti desni.

 # tabela_kod primer_drevesa;;
 - : (char * bit list) list =
 [('a', [O]); ('r', [I; O]); ('b', [I; I; O]); ('d', [I; I; I; O]);
  ('k', [I; I; I; I])]
[*----------------------------------------------------------------------------*)

let tabela_kod drevo =
  let rec aux drevo koda =
    match drevo with
    | List znak -> [(znak, koda)]
    | Vozlisce (levo, desno) -> aux levo (koda @ [O]) @ aux desno (koda @ [I])
  in
  aux drevo []

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `kodiraj : drevo -> string -> bit list`, ki besedilo
 zakodira tako, da stakne kode vseh njegovih znakov.

 # v_niz_bitov (kodiraj primer_drevesa "abra");;
 - : string = "0110100"
[*----------------------------------------------------------------------------*)

let kodiraj drevo niz =
  let tabela = tabela_kod drevo in
  let rec aux znaki =
    match znaki with
    | [] -> []
    | x :: xs ->
        List.assoc x tabela @ aux xs
  in
  aux (znaki niz)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `dekodiraj : drevo -> bit list -> string`, ki zaporedje
 bitov prebere nazaj v besedilo. Odvečne bite na koncu, ki ne sestavljajo
 cele kode, naj prezre.

 # dekodiraj primer_drevesa (iz_niza_bitov "110011100");;
 - : string = "bada"
[*----------------------------------------------------------------------------*)

let dekodiraj drevo biti =
  let rec aux biti =
    match biti with
    | [] -> []
    | _ ->
        let rec beri drevo biti =
          match drevo, biti with
          | List znak, _ -> znak :: aux biti
          | Vozlisce (levo, desno), O :: xs -> beri levo xs
          | Vozlisce (levo, desno), I :: xs -> beri desno xs
          | Vozlisce _, [] -> []
        in
        beri drevo biti
  in
  iz_znakov (aux biti)

(*----------------------------------------------------------------------------*
 ## Postopek gradnje drevesa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `frekvence : string -> (char * int) list`, ki za vsak znak
 v besedilu vrne število njegovih pojavitev. Seznam naj bo urejen po znakih.

 # frekvence "abrakadabra";;
 - : (char * int) list = [('a', 5); ('b', 2); ('d', 1); ('k', 1); ('r', 2)]
[*----------------------------------------------------------------------------*)

let frekvence niz =
  let rec aux niz ze_videni =
    match znaki niz with
    | [] -> ze_videni
    | x :: xs ->
        if List.mem x (List.map fst ze_videni) then
          aux (iz_znakov xs)
            (List.map (fun (c, n) ->
              if c = x then (c, n + 1) else (c, n)
              ) ze_videni)
        else
          aux (iz_znakov xs) ((x, 1) :: ze_videni)
  in
  List.sort compare (aux niz []) 

(*----------------------------------------------------------------------------*
 Najboljše drevo za besedilo zgradimo iz frekvenc njegovih znakov. Utež
 drevesa je vsota frekvenc znakov v njegovih listih.

 1. Za vsak znak naredimo list, katerega utež je frekvenca znaka.
 2. Dokler imamo več kot eno drevo, vzamemo dve drevesi z najmanjšima
    utežema in ju združimo v vozlišče. Lažje drevo gre levo, utež novega
    drevesa pa je vsota obeh uteži.
 3. Ko ostane eno samo drevo, je to iskano drevo.

 Če imata dve drevesi enako utež, je lažje tisto, ki vsebuje manjši znak. Za
 "abrakadabra" postopek teče takole (drevesa z več listi smo zapisali z
 oglatimi oklepaji):

   drevesa (utež, drevo)                       združimo
   (1, d) (1, k) (2, b) (2, r) (5, a)          d in k
   (2, b) (2, [d k]) (2, r) (5, a)             b in [d k]
   (2, r) (4, [b [d k]]) (5, a)                r in [b [d k]]
   (5, a) (6, [r [b [d k]]])                   a in [r [b [d k]]]

 V drugem koraku moramo večkrat vzeti najlažje drevo in dodati novo. Za to
 bomo potrebovali prioritetno vrsto.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Prioritetne vrste

 Prioritetna vrsta hrani elemente, ven pa jih jemljemo od najmanjšega proti
 največjemu. Elemente primerja z `<=`, zato deluje za poljuben tip.
[*----------------------------------------------------------------------------*)

module type PRIORITETNA_VRSTA = sig
  type 'a t

  val prazna : 'a t
  val vstavi : 'a -> 'a t -> 'a t

  val odstrani_najmanjsega : 'a t -> ('a * 'a t) option
  (** Vrne najmanjši element in vrsto brez njega oziroma `None`, če je vrsta
      prazna. *)

  val zlij : 'a t -> 'a t -> 'a t
  (** Vrne vrsto z elementi obeh vrst. *)
end

(*----------------------------------------------------------------------------*
 ### Urejeni seznami

 Sestavite modul `UrejenSeznam`, ki prioritetno vrsto predstavi z naraščajoče
 urejenim seznamom.

 # UrejenSeznam.(prazna |> vstavi 3 |> vstavi 1 |> vstavi 2
                 |> odstrani_najmanjsega);;
 - : (int * int UrejenSeznam.t) option = Some (1, <abstr>)
[*----------------------------------------------------------------------------*)

module UrejenSeznam : PRIORITETNA_VRSTA = struct
  type 'a t = 'a list

  let prazna = []
  let rec vstavi el vrsta = 
    match vrsta with
  | [] -> [el]
  | x :: xs ->
      if el <= x then el :: vrsta
      else x :: vstavi el xs

  let odstrani_najmanjsega vrsta = 
    match vrsta with
    | [] -> None
    | x :: xs -> Some (x, xs)

  let rec zlij vrsta1 vrsta2 = 
    match vrsta1, vrsta2 with
    | [], [] -> []
    | [], x :: xs -> vrsta2
    | x :: xs, [] -> vrsta1
    | x :: xs, y :: ys -> 
      if x <= y then [x] @ (zlij xs vrsta2)
      else [y] @ (zlij vrsta1 ys)

end

(*----------------------------------------------------------------------------*
 ### Levičarske kopice

 Pri urejenem seznamu vstavljanje porabi O(n) časa. Hitrejšo prioritetno
 vrsto dobimo s kopico. Kopica je dvojiško drevo, v katerem je vrednost v
 vsakem vozlišču manjša ali enaka vrednostma v obeh otrocih, zato je
 najmanjši element v korenu. Element v kopico vstavimo tako, da jo zlijemo s
 kopico z enim samim elementom, najmanjši element pa odstranimo tako, da
 zlijemo obe poddrevesi korena.

 Zlivanje se spušča le po desnih poteh kopic, zato želimo, da so te kratke.
 Rang kopice je število vozlišč na njeni skrajno desni poti, rang prazne
 kopice pa je 0. Kopica je levičarska, če je v vsakem vozlišču rang levega
 otroka večji ali enak rangu desnega. Desna pot levičarske kopice z n
 elementi ima tako največ log2(n + 1) vozlišč. Koliko elementov je v levem
 in koliko v desnem poddrevesu, pri tem ni pomembno.

 V vsakem vozlišču hranimo rang, levo poddrevo, vrednost in desno poddrevo.
 Spodaj je primer levičarske kopice, pri kateri so rangi zapisani v
 oklepajih:

          1 (2)
         /     \
      3 (2)   4 (1)
      /   \
   5 (1)  8 (1)
[*----------------------------------------------------------------------------*)

type 'a kopica = Prazna | Sestavljena of int * 'a kopica * 'a * 'a kopica

let primer_kopice =
  let list x = Sestavljena (1, Prazna, x, Prazna) in
  Sestavljena (2, Sestavljena (2, list 5, 3, list 8), 1, list 4)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `rang : 'a kopica -> int`, ki v času O(1) vrne rang
 kopice.
[*----------------------------------------------------------------------------*)

let rang kopica = 
  match kopica with
  | Prazna -> 0
  | Sestavljena (x, _, _, _) -> x

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `je_levicarska_kopica : 'a kopica -> bool`, ki preveri, ali
 je drevo levičarska kopica s pravilno zapisanimi rangi.

 # je_levicarska_kopica primer_kopice;;
 - : bool = true
 # je_levicarska_kopica
     (Sestavljena (1, Prazna, 1, Sestavljena (1, Prazna, 3, Prazna)));;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec je_levicarska_kopica kopica =
  match kopica with
  | Prazna -> true
  | Sestavljena (r, levo, x, desno) ->
      let urejena =
        match levo, desno with
        | Prazna, Prazna -> true
        | Sestavljena (_, _, y, _), Prazna -> x <= y
        | Prazna, Sestavljena (_, _, z, _) -> x <= z
        | Sestavljena (_, _, y, _), Sestavljena (_, _, z, _) ->
            x <= y && x <= z
      in
      urejena
      && rang levo >= rang desno
      && r = rang desno + 1
      && je_levicarska_kopica levo
      && je_levicarska_kopica desno

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `sestavi : 'a -> 'a kopica -> 'a kopica -> 'a kopica`, ki
 iz vrednosti in dveh levičarskih kopic sestavi levičarsko kopico z dano
 vrednostjo v korenu. Predpostavite lahko, da vrednost ni večja od korenov
 obeh kopic.

 # sestavi 1 (Sestavljena (1, Prazna, 4, Prazna))
     (Sestavljena (2, Sestavljena (1, Prazna, 5, Prazna), 3,
                   Sestavljena (1, Prazna, 8, Prazna))) = primer_kopice;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let sestavi koren kopica1 kopica2 =
  if rang kopica1 >= rang kopica2 then
    Sestavljena (rang kopica2 + 1, kopica1, koren, kopica2)
  else
    Sestavljena (rang kopica1 + 1, kopica2, koren, kopica1)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `zlij : 'a kopica -> 'a kopica -> 'a kopica`, ki zlije dve
 levičarski kopici v eno. Funkcija naj porabi O(log n) časa, kjer je n skupno
 število elementov v obeh kopicah.

 Če zlijemo `primer_kopice` in kopico

      6 (1)
      /
   7 (1)

 dobimo

             1 (2)
            /     \
       3 (2)       4 (1)
       /   \      /
    5 (1) 8 (1)  6 (1)
                 /
              7 (1)
[*----------------------------------------------------------------------------*)

let rec zlij kopica1 kopica2 =
  match kopica1, kopica2 with
  | Prazna, kopica | kopica, Prazna -> kopica
  | Sestavljena (_, levo1, x, desno1),
    Sestavljena (_, levo2, y, desno2) ->
      if x <= y then
        sestavi x levo1 (zlij desno1 kopica2)
      else
        sestavi y levo2 (zlij kopica1 desno2)

(*----------------------------------------------------------------------------*
 Sestavite modul `LevicarskaKopica`, ki prioritetno vrsto predstavi z
 levičarsko kopico.
[*----------------------------------------------------------------------------*)

module LevicarskaKopica : PRIORITETNA_VRSTA = struct
  type 'a t = 'a kopica

  let prazna = Prazna
  let vstavi el kopica = 
    zlij (Sestavljena (1, Prazna, el, Prazna)) kopica

  let odstrani_najmanjsega kopica = 
    match kopica with
  | Prazna -> None
  | Sestavljena (_, levo, x, desno) ->
      Some (x, zlij levo desno)

  let zlij kopica1 kopica2 = 
    zlij kopica1 kopica2

end

(*----------------------------------------------------------------------------*
 ### Urejanje s prioritetno vrsto

 Sestavite funktor `Uredi`, ki sprejme modul s signaturo `PRIORITETNA_VRSTA` in
 vsebuje funkcijo `uredi : 'a list -> 'a list`, ki seznam uredi s pomočjo
 prioritetne vrste.

 # UrediSKopico.uredi [5; 3; 8; 1; 4; 1];;
 - : int list = [1; 1; 3; 4; 5; 8]
[*----------------------------------------------------------------------------*)

module Uredi (V : PRIORITETNA_VRSTA) = struct
  let uredi seznam = 
    let vrsta =
      List.fold_left (fun vrsta el -> V.vstavi el vrsta) V.prazna seznam
    in
    let rec aux vrsta =
      match V.odstrani_najmanjsega vrsta with
      | None -> []
      | Some (x, nova_vrsta) -> x :: aux nova_vrsta
    in
    aux vrsta

end

module UrediSSeznamom = Uredi (UrejenSeznam)
module UrediSKopico = Uredi (LevicarskaKopica)

(*----------------------------------------------------------------------------*
 ## Huffmanovo drevo

 Sestavite funktor `Huffman`, ki sprejme modul s signaturo
 `PRIORITETNA_VRSTA` in vsebuje funkcijo
 `zgradi_drevo : (char * int) list -> drevo`, ki iz frekvenc zgradi drevo po
 zgoraj opisanem postopku. Predpostavite lahko, da sta v seznamu vsaj dva
 znaka. Namig: v vrsto dajajte trojice `(utez, najmanjsi_znak, drevo)`, saj
 jih OCaml primerja najprej po uteži, nato pa po znaku.

 # HuffmanSKopico.zgradi_drevo (frekvence "abrakadabra") = primer_drevesa;;
 - : bool = true
 # HuffmanSKopico.zgradi_drevo (frekvence "mississippi");;
 - : drevo =
 Vozlisce (List 's', Vozlisce (Vozlisce (List 'm', List 'p'), List 'i'))
[*----------------------------------------------------------------------------*)

module Huffman (V : PRIORITETNA_VRSTA) = struct
  let zgradi_drevo seznam = 
    let vrsta =
      List.fold_left
        (fun vrsta (znak, utez) ->
          V.vstavi (utez, znak, List znak) vrsta)
        V.prazna seznam
    in
    let rec aux vrsta =
      match V.odstrani_najmanjsega vrsta with
      | None -> failwith "Napaka"
      | Some ((utez1, znak1, drevo1), vrsta1) ->
          match V.odstrani_najmanjsega vrsta1 with
          | None -> failwith "Napaka"
          | Some ((utez2, znak2, drevo2), vrsta2) ->
              match V.odstrani_najmanjsega vrsta2 with
              | None -> Vozlisce (drevo1, drevo2)
              | Some _ ->
                  let novo =
                    (utez1 + utez2, min znak1 znak2,
                     Vozlisce (drevo1, drevo2))
                  in
                  aux (V.vstavi novo vrsta2)
    in
    aux vrsta

end

module HuffmanSSeznamom = Huffman (UrejenSeznam)
module HuffmanSKopico = Huffman (LevicarskaKopica)

let zgradi_drevo = HuffmanSKopico.zgradi_drevo

(*----------------------------------------------------------------------------*
 ## Stiskanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `dolzina_kodiranja : (char * int) list -> drevo -> int`,
 ki vrne število bitov, ki jih zasede besedilo z danimi frekvencami,
 zakodirano z danim drevesom.

 # dolzina_kodiranja (frekvence "abrakadabra") primer_drevesa;;
 - : int = 23
[*----------------------------------------------------------------------------*)

let dolzina_kodiranja seznam drevo = 
  let tabela = tabela_kod drevo in
  List.fold_left
    (fun vsota (znak, frekvenca) ->
      vsota + frekvenca * List.length (List.assoc znak tabela))
    0 seznam

(*----------------------------------------------------------------------------*
 Sestavite funkciji `stisni : string -> drevo * bit list`, ki zgradi drevo za
 dano besedilo in vrne drevo ter zakodirano besedilo, in
 `razsiri : drevo * bit list -> string`, ki stisnjeno besedilo razširi nazaj.

 # let _, biti = stisni "abrakadabra" in v_niz_bitov biti;;
 - : string = "01101001111011100110100"
 # razsiri (stisni "abrakadabra");;
 - : string = "abrakadabra"
[*----------------------------------------------------------------------------*)

let stisni niz = 
  let seznam = frekvence niz in
  let drevo = HuffmanSKopico.zgradi_drevo seznam in
  (drevo, kodiraj drevo niz)

let razsiri (drevo, biti) = 
  dekodiraj drevo biti

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `razmerje_stiskanja : string -> float`, ki vrne razmerje
 med dolžino stisnjenega besedila in dolžino besedila, zapisanega z 8 biti na
 znak.

 # razmerje_stiskanja "aaaabbcd";;
 - : float = 0.21875
[*----------------------------------------------------------------------------*)

let razmerje_stiskanja niz = 
  let (_, biti) = stisni niz in
  float_of_int (List.length biti) /.
  float_of_int (8 * String.length niz)

(* Pripomočki za testiranje - ignorirajte *)

type ('a, 'b) test_case = {
  input : 'a;
  expected_output : ('b, exn) result;
  description : string;
}

type ('a, 'b) test_suite = {
  cases : ('a, 'b) test_case list;
  fn : 'a -> 'b;
  name : string;
}

let uncurry_f_2 f (x, y) = f x y
let uncurry_f_3 f (x, y, z) = f x y z
let uncurry_f_4 f (x, y, z, w) = f x y z w
let uncurry_f_5 f (x, y, z, w, v) = f x y z w v

let tester suite =
  List.fold_left
    (fun (correct, incorrect) { input; expected_output; description } ->
      try
        let actual_output = suite.fn input in
        match expected_output with
        | Ok expected_output when actual_output = expected_output ->
            (correct + 1, incorrect)
        | _ -> (correct, description :: incorrect)
      with e -> (
        match expected_output with
        | Error expected_exception when e = expected_exception ->
            (correct + 1, incorrect)
        | _ ->
            ( correct,
              (description ^ " (exception: " ^ Printexc.to_string e ^ ")")
              :: incorrect )))
    (0, []) suite.cases

let run_suite suite verbose =
  let correct, incorrect = tester suite in
  if verbose then (
    Printf.printf "Test suite: %s\n" suite.name;
    Printf.printf "Correct: %d\n" correct;
    Printf.printf "Incorrect: %d\n" (List.length incorrect);
    List.iter (Printf.printf "- %s\n") incorrect;
    Printf.printf "\n";
    Printf.printf "Test suite: %s - Correct: %d, Incorrect: %d, Total: %f\n"
      suite.name correct (List.length incorrect)
      (float_of_int correct /. float_of_int (correct + List.length incorrect));
    Printf.printf "\n")
  else
    Printf.printf "Test suite: %s - Correct: %d, Incorrect: %d, Total: %f\n"
      suite.name correct (List.length incorrect)
      (float_of_int correct /. float_of_int (correct + List.length incorrect));
  (correct, List.length incorrect)

(* Testiranje *)

let enojna x = Sestavljena (1, Prazna, x, Prazna)

let drevo_mississippi =
  Vozlisce (List 's', Vozlisce (Vozlisce (List 'm', List 'p'), List 'i'))

let drevo_abcd =
  Vozlisce (Vozlisce (List 'a', List 'b'), Vozlisce (List 'c', List 'd'))

let biti_abrakadabra =
  [ O; I; I; O; I; O; O; I; I; I; I; O; I; I; I; O; O; I; I; O; I; O; O ]

let biti_mississippi =
  [ I; O; O; I; I; O; O; I; I; O; O; I; I; I; O; I; I; O; I; I; I ]

let besedilo_danes = "danes je lep dan"

let frekvence_danes =
  [
    (' ', 3); ('a', 2); ('d', 2); ('e', 3); ('j', 1); ('l', 1); ('n', 2);
    ('p', 1); ('s', 1);
  ]

let drevo_danes =
  Vozlisce
    ( Vozlisce (List 'e', Vozlisce (List 'a', List 'd')),
      Vozlisce
        ( Vozlisce (Vozlisce (List 'j', List 'l'), List 'n'),
          Vozlisce (Vozlisce (List 'p', List 's'), List ' ') ) )

let biti_danes =
  [
    O; I; I; O; I; O; I; O; I; O; O; I; I; O; I; I; I; I; I; O; O; O; O; O; I;
    I; I; I; O; O; I; O; O; I; I; O; O; I; I; I; O; I; I; O; I; O; I; O; I;
  ]

(* Vrsta, ki elemente izmenično dodaja v dve vrsti, ob odstranjevanju pa ju
   zlije. Z njo funkcijo zlij preverimo preko funkcije uredi. *)
module DvojnaVrsta (V : PRIORITETNA_VRSTA) : PRIORITETNA_VRSTA = struct
  type 'a t = 'a V.t * 'a V.t

  let prazna = (V.prazna, V.prazna)
  let vstavi x (v1, v2) = (v2, V.vstavi x v1)

  let odstrani_najmanjsega (v1, v2) =
    match V.odstrani_najmanjsega (V.zlij v1 v2) with
    | None -> None
    | Some (x, v) -> Some (x, (v, V.prazna))

  let zlij (v1, v2) (v3, v4) = (V.zlij v1 v3, V.zlij v2 v4)
end

module UrediZZlivanjemSeznamov = Uredi (DvojnaVrsta (UrejenSeznam))
module UrediZZlivanjemKopic = Uredi (DvojnaVrsta (LevicarskaKopica))

let v_niz_bitov_suite =
  {
    name = "v_niz_bitov";
    fn = v_niz_bitov;
    cases =
      [
        {
          input = [];
          expected_output = Ok "";
          description = "Prazen seznam bitov";
        };
        { input = [ O ]; expected_output = Ok "0"; description = "En bit O" };
        { input = [ I ]; expected_output = Ok "1"; description = "En bit I" };
        {
          input = [ O; I; I; O ];
          expected_output = Ok "0110";
          description = "Bitov 0110";
        };
        {
          input = biti_abrakadabra;
          expected_output = Ok "01101001111011100110100";
          description = "Zakodirano besedilo abrakadabra";
        };
      ];
  }

let iz_niza_bitov_suite =
  {
    name = "iz_niza_bitov";
    fn = iz_niza_bitov;
    cases =
      [
        { input = ""; expected_output = Ok []; description = "Prazen niz" };
        { input = "0"; expected_output = Ok [ O ]; description = "Niz 0" };
        { input = "1"; expected_output = Ok [ I ]; description = "Niz 1" };
        {
          input = "0110";
          expected_output = Ok [ O; I; I; O ];
          description = "Niz 0110";
        };
        {
          input = "01101001111011100110100";
          expected_output = Ok biti_abrakadabra;
          description = "Zakodirano besedilo abrakadabra";
        };
      ];
  }

let tabela_kod_suite =
  {
    name = "tabela_kod";
    fn = tabela_kod;
    cases =
      [
        {
          input = primer_drevesa;
          expected_output =
            Ok
              [
                ('a', [ O ]);
                ('r', [ I; O ]);
                ('b', [ I; I; O ]);
                ('d', [ I; I; I; O ]);
                ('k', [ I; I; I; I ]);
              ];
          description = "Drevo za abrakadabra";
        };
        {
          input = Vozlisce (List 'x', List 'y');
          expected_output = Ok [ ('x', [ O ]); ('y', [ I ]) ];
          description = "Drevo z dvema listoma";
        };
        {
          input = drevo_abcd;
          expected_output =
            Ok
              [
                ('a', [ O; O ]);
                ('b', [ O; I ]);
                ('c', [ I; O ]);
                ('d', [ I; I ]);
              ];
          description = "Uravnoteženo drevo s štirimi listi";
        };
        {
          input = drevo_mississippi;
          expected_output =
            Ok
              [
                ('s', [ O ]);
                ('m', [ I; O; O ]);
                ('p', [ I; O; I ]);
                ('i', [ I; I ]);
              ];
          description = "Drevo za mississippi";
        };
        {
          input = drevo_danes;
          expected_output =
            Ok
              [
                ('e', [ O; O ]);
                ('a', [ O; I; O ]);
                ('d', [ O; I; I ]);
                ('j', [ I; O; O; O ]);
                ('l', [ I; O; O; I ]);
                ('n', [ I; O; I ]);
                ('p', [ I; I; O; O ]);
                ('s', [ I; I; O; I ]);
                (' ', [ I; I; I ]);
              ];
          description = "Drevo za stavek s presledki";
        };
      ];
  }

let kodiraj_suite =
  {
    name = "kodiraj";
    fn = uncurry_f_2 kodiraj;
    cases =
      [
        {
          input = (primer_drevesa, "abra");
          expected_output = Ok [ O; I; I; O; I; O; O ];
          description = "Kodiranje besede abra";
        };
        {
          input = (primer_drevesa, "");
          expected_output = Ok [];
          description = "Kodiranje praznega besedila";
        };
        {
          input = (primer_drevesa, "abrakadabra");
          expected_output = Ok biti_abrakadabra;
          description = "Kodiranje besedila abrakadabra";
        };
        {
          input = (drevo_mississippi, "mississippi");
          expected_output = Ok biti_mississippi;
          description = "Kodiranje besedila mississippi";
        };
        {
          input = (drevo_abcd, "dcba");
          expected_output = Ok [ I; I; I; O; O; I; O; O ];
          description = "Kodiranje z uravnoteženim drevesom";
        };
        {
          input = (drevo_danes, besedilo_danes);
          expected_output = Ok biti_danes;
          description = "Kodiranje stavka s presledki";
        };
      ];
  }

let dekodiraj_suite =
  {
    name = "dekodiraj";
    fn = uncurry_f_2 dekodiraj;
    cases =
      [
        {
          input = (primer_drevesa, [ I; I; O; O; I; I; I; O; O ]);
          expected_output = Ok "bada";
          description = "Dekodiranje besede bada";
        };
        {
          input = (primer_drevesa, []);
          expected_output = Ok "";
          description = "Dekodiranje praznega seznama bitov";
        };
        {
          input = (primer_drevesa, biti_abrakadabra);
          expected_output = Ok "abrakadabra";
          description = "Dekodiranje besedila abrakadabra";
        };
        {
          input = (drevo_mississippi, biti_mississippi);
          expected_output = Ok "mississippi";
          description = "Dekodiranje besedila mississippi";
        };
        {
          input = (primer_drevesa, [ I; I; O; O; I; I; I; O; O; I; I ]);
          expected_output = Ok "bada";
          description = "Odvečni biti na koncu";
        };
        {
          input = (primer_drevesa, [ I; I; I ]);
          expected_output = Ok "";
          description = "Samo nepopolna koda";
        };
        {
          input = (drevo_danes, biti_danes);
          expected_output = Ok besedilo_danes;
          description = "Dekodiranje stavka s presledki";
        };
      ];
  }

let frekvence_suite =
  {
    name = "frekvence";
    fn = frekvence;
    cases =
      [
        {
          input = "abrakadabra";
          expected_output =
            Ok [ ('a', 5); ('b', 2); ('d', 1); ('k', 1); ('r', 2) ];
          description = "Frekvence v besedilu abrakadabra";
        };
        {
          input = "";
          expected_output = Ok [];
          description = "Prazno besedilo";
        };
        {
          input = "aaa";
          expected_output = Ok [ ('a', 3) ];
          description = "En sam znak";
        };
        {
          input = "mississippi";
          expected_output = Ok [ ('i', 4); ('m', 1); ('p', 2); ('s', 4) ];
          description = "Frekvence v besedilu mississippi";
        };
        {
          input = "zyx y";
          expected_output = Ok [ (' ', 1); ('x', 1); ('y', 2); ('z', 1) ];
          description = "Znaki v obratnem vrstnem redu in presledek";
        };
        {
          input = besedilo_danes;
          expected_output = Ok frekvence_danes;
          description = "Stavek s presledki";
        };
      ];
  }

let rang_suite =
  {
    name = "rang";
    fn = rang;
    cases =
      [
        {
          input = Prazna;
          expected_output = Ok 0;
          description = "Prazna kopica";
        };
        {
          input = enojna 4;
          expected_output = Ok 1;
          description = "Kopica z enim elementom";
        };
        {
          input = primer_kopice;
          expected_output = Ok 2;
          description = "Kopica primer_kopice";
        };
        {
          input =
            Sestavljena
              ( 3,
                Sestavljena
                  (2, enojna 30, 13, Sestavljena (1, enojna 56, 45, Prazna)),
                9,
                Sestavljena (2, enojna 52, 18, enojna 36) );
          expected_output = Ok 3;
          description = "Kopica z rangom 3";
        };
      ];
  }

let je_levicarska_kopica_suite =
  {
    name = "je_levicarska_kopica";
    fn = je_levicarska_kopica;
    cases =
      [
        {
          input = Prazna;
          expected_output = Ok true;
          description = "Prazna kopica";
        };
        {
          input = enojna 7;
          expected_output = Ok true;
          description = "Kopica z enim elementom";
        };
        {
          input = primer_kopice;
          expected_output = Ok true;
          description = "Kopica primer_kopice";
        };
        {
          input =
            Sestavljena
              ( 2,
                Sestavljena (2, enojna 5, 3, enojna 8),
                1,
                Sestavljena (1, Sestavljena (1, enojna 7, 6, Prazna), 4, Prazna)
              );
          expected_output = Ok true;
          description = "Kopica z več elementi v desnem poddrevesu";
        };
        {
          input = Sestavljena (2, Prazna, 1, enojna 3);
          expected_output = Ok false;
          description = "Desni otrok ima večji rang";
        };
        {
          input = Sestavljena (1, enojna 5, 3, enojna 8);
          expected_output = Ok false;
          description = "Napačno zapisan rang";
        };
        {
          input = Sestavljena (2, enojna 5, 6, enojna 8);
          expected_output = Ok false;
          description = "Koren je večji od otroka";
        };
        {
          input =
            Sestavljena
              (2, Sestavljena (2, enojna 2, 3, enojna 8), 1, enojna 4);
          expected_output = Ok false;
          description = "Vozlišče globlje v drevesu je večje od otroka";
        };
        {
          input =
            Sestavljena (2, Sestavljena (2, Prazna, 3, enojna 5), 1, enojna 4);
          expected_output = Ok false;
          description = "Globlje v drevesu ima desni otrok večji rang";
        };
      ];
  }

let sestavi_suite =
  {
    name = "sestavi";
    fn = uncurry_f_3 sestavi;
    cases =
      [
        {
          input = (1, enojna 4, Sestavljena (2, enojna 5, 3, enojna 8));
          expected_output = Ok primer_kopice;
          description = "Kopica z večjim rangom gre levo";
        };
        {
          input = (1, Sestavljena (2, enojna 5, 3, enojna 8), enojna 4);
          expected_output = Ok primer_kopice;
          description = "Kopica z večjim rangom je že levo";
        };
        {
          input = (0, Prazna, enojna 5);
          expected_output = Ok (Sestavljena (1, enojna 5, 0, Prazna));
          description = "Prazna kopica na levi";
        };
        {
          input = (0, enojna 5, Prazna);
          expected_output = Ok (Sestavljena (1, enojna 5, 0, Prazna));
          description = "Prazna kopica na desni";
        };
        {
          input = (2, Prazna, Prazna);
          expected_output = Ok (enojna 2);
          description = "Dve prazni kopici";
        };
        {
          input = (0, primer_kopice, Sestavljena (1, enojna 7, 6, Prazna));
          expected_output =
            Ok
              (Sestavljena
                 (2, primer_kopice, 0, Sestavljena (1, enojna 7, 6, Prazna)));
          description = "Večji kopici";
        };
      ];
  }

let zlij_suite =
  {
    name = "zlij";
    fn = uncurry_f_2 zlij;
    cases =
      [
        {
          input = (Prazna, Prazna);
          expected_output = Ok Prazna;
          description = "Dve prazni kopici";
        };
        {
          input = (Prazna, primer_kopice);
          expected_output = Ok primer_kopice;
          description = "Prva kopica je prazna";
        };
        {
          input = (primer_kopice, Prazna);
          expected_output = Ok primer_kopice;
          description = "Druga kopica je prazna";
        };
        {
          input = (enojna 1, enojna 2);
          expected_output = Ok (Sestavljena (1, enojna 2, 1, Prazna));
          description = "Dve kopici z enim elementom";
        };
        {
          input = (enojna 2, enojna 1);
          expected_output = Ok (Sestavljena (1, enojna 2, 1, Prazna));
          description = "Dve kopici z enim elementom, manjši drugi";
        };
        {
          input = (primer_kopice, Sestavljena (1, enojna 7, 6, Prazna));
          expected_output =
            Ok
              (Sestavljena
                 ( 2,
                   Sestavljena (2, enojna 5, 3, enojna 8),
                   1,
                   Sestavljena
                     (1, Sestavljena (1, enojna 7, 6, Prazna), 4, Prazna) ));
          description = "Primer iz navodil";
        };
        {
          input =
            ( Sestavljena
                ( 2,
                  Sestavljena (1, enojna 25, 7, Prazna),
                  0,
                  Sestavljena (1, enojna 11, 4, Prazna) ),
              Sestavljena
                (2, Sestavljena (1, enojna 58, 56, Prazna), 13, enojna 20) );
          expected_output =
            Ok
              (Sestavljena
                 ( 2,
                   Sestavljena
                     ( 2,
                       Sestavljena
                         ( 2,
                           Sestavljena (1, enojna 58, 56, Prazna),
                           13,
                           enojna 20 ),
                       4,
                       enojna 11 ),
                   0,
                   Sestavljena (1, enojna 25, 7, Prazna) ));
          description = "Večji kopici (1)";
        };
        {
          input =
            ( Sestavljena
                ( 2,
                  Sestavljena (2, enojna 48, 24, enojna 45),
                  5,
                  Sestavljena (1, enojna 33, 22, Prazna) ),
              Sestavljena (2, enojna 27, 4, enojna 11) );
          expected_output =
            Ok
              (Sestavljena
                 ( 2,
                   Sestavljena
                     ( 2,
                       Sestavljena (2, enojna 48, 24, enojna 45),
                       5,
                       Sestavljena
                         (1, Sestavljena (1, enojna 33, 22, Prazna), 11, Prazna)
                     ),
                   4,
                   enojna 27 ));
          description = "Večji kopici (2)";
        };
        {
          input =
            ( Sestavljena
                (2, Sestavljena (2, enojna 43, 31, enojna 41), 23, enojna 58),
              Sestavljena
                (2, Sestavljena (2, enojna 19, 18, enojna 32), 7, enojna 20) );
          expected_output =
            Ok
              (Sestavljena
                 ( 2,
                   Sestavljena (2, enojna 19, 18, enojna 32),
                   7,
                   Sestavljena
                     ( 1,
                       Sestavljena
                         ( 2,
                           Sestavljena (2, enojna 43, 31, enojna 41),
                           23,
                           enojna 58 ),
                       20,
                       Prazna ) ));
          description = "Večji kopici (3)";
        };
      ];
  }

let uredi_s_seznamom_suite =
  {
    name = "UrediSSeznamom.uredi";
    fn = UrediSSeznamom.uredi;
    cases =
      [
        { input = []; expected_output = Ok []; description = "Prazen seznam" };
        {
          input = [ 1 ];
          expected_output = Ok [ 1 ];
          description = "En element";
        };
        {
          input = [ 5; 3; 8; 1; 4; 1 ];
          expected_output = Ok [ 1; 1; 3; 4; 5; 8 ];
          description = "Seznam s ponovitvami";
        };
        {
          input = List.init 200 (fun i -> i * 37 mod 211);
          expected_output =
            Ok (List.sort compare (List.init 200 (fun i -> i * 37 mod 211)));
          description = "Dvesto števil";
        };
      ];
  }

let uredi_s_kopico_suite =
  {
    name = "UrediSKopico.uredi";
    fn = UrediSKopico.uredi;
    cases =
      [
        { input = []; expected_output = Ok []; description = "Prazen seznam" };
        {
          input = [ 1 ];
          expected_output = Ok [ 1 ];
          description = "En element";
        };
        {
          input = [ 5; 3; 8; 1; 4; 1 ];
          expected_output = Ok [ 1; 1; 3; 4; 5; 8 ];
          description = "Seznam s ponovitvami";
        };
        {
          input = List.init 200 (fun i -> i * 37 mod 211);
          expected_output =
            Ok (List.sort compare (List.init 200 (fun i -> i * 37 mod 211)));
          description = "Dvesto števil";
        };
      ];
  }

let zlij_seznamov_suite =
  {
    name = "UrejenSeznam.zlij (preko funkcije uredi)";
    fn = UrediZZlivanjemSeznamov.uredi;
    cases =
      [
        {
          input = [ 2; 1; 4; 3; 6; 5 ];
          expected_output = Ok [ 1; 2; 3; 4; 5; 6 ];
          description = "Vrsti 2, 4, 6 in 1, 3, 5";
        };
        {
          input = [ 3 ];
          expected_output = Ok [ 3 ];
          description = "Zlivanje s prazno vrsto";
        };
        {
          input = [ 1; 1; 1 ];
          expected_output = Ok [ 1; 1; 1 ];
          description = "Enaki elementi";
        };
        {
          input = List.init 200 (fun i -> i * 37 mod 211);
          expected_output =
            Ok (List.sort compare (List.init 200 (fun i -> i * 37 mod 211)));
          description = "Dvesto števil";
        };
      ];
  }

let zlij_kopic_suite =
  {
    name = "LevicarskaKopica.zlij (preko funkcije uredi)";
    fn = UrediZZlivanjemKopic.uredi;
    cases =
      [
        {
          input = [ 2; 1; 4; 3; 6; 5 ];
          expected_output = Ok [ 1; 2; 3; 4; 5; 6 ];
          description = "Vrsti 2, 4, 6 in 1, 3, 5";
        };
        {
          input = [ 3 ];
          expected_output = Ok [ 3 ];
          description = "Zlivanje s prazno vrsto";
        };
        {
          input = [ 1; 1; 1 ];
          expected_output = Ok [ 1; 1; 1 ];
          description = "Enaki elementi";
        };
        {
          input = List.init 200 (fun i -> i * 37 mod 211);
          expected_output =
            Ok (List.sort compare (List.init 200 (fun i -> i * 37 mod 211)));
          description = "Dvesto števil";
        };
      ];
  }

let zgradi_drevo_primeri =
  [
    {
      input = [ ('a', 5); ('b', 2); ('d', 1); ('k', 1); ('r', 2) ];
      expected_output = Ok primer_drevesa;
      description = "Frekvence za abrakadabra";
    };
    {
      input = [ ('r', 2); ('a', 5); ('k', 1); ('b', 2); ('d', 1) ];
      expected_output = Ok primer_drevesa;
      description = "Frekvence za abrakadabra v drugem vrstnem redu";
    };
    {
      input = [ ('i', 4); ('m', 1); ('p', 2); ('s', 4) ];
      expected_output = Ok drevo_mississippi;
      description = "Frekvence za mississippi";
    };
    {
      input = [ ('a', 1); ('b', 1) ];
      expected_output = Ok (Vozlisce (List 'a', List 'b'));
      description = "Dva enako pogosta znaka";
    };
    {
      input = [ ('a', 3); ('b', 1) ];
      expected_output = Ok (Vozlisce (List 'b', List 'a'));
      description = "Dva različno pogosta znaka";
    };
    {
      input = [ ('a', 4); ('b', 2); ('c', 1); ('d', 1) ];
      expected_output =
        Ok
          (Vozlisce
             (List 'a', Vozlisce (List 'b', Vozlisce (List 'c', List 'd'))));
      description = "Frekvence 4, 2, 1, 1";
    };
    {
      input = [ ('a', 4); ('b', 4); ('c', 4); ('d', 4) ];
      expected_output = Ok drevo_abcd;
      description = "Štirje enako pogosti znaki";
    };
    {
      input = frekvence_danes;
      expected_output = Ok drevo_danes;
      description = "Frekvence za stavek s presledki";
    };
  ]

let huffman_s_seznamom_suite =
  {
    name = "HuffmanSSeznamom.zgradi_drevo";
    fn = HuffmanSSeznamom.zgradi_drevo;
    cases = zgradi_drevo_primeri;
  }

let huffman_s_kopico_suite =
  {
    name = "HuffmanSKopico.zgradi_drevo";
    fn = HuffmanSKopico.zgradi_drevo;
    cases = zgradi_drevo_primeri;
  }

let dolzina_kodiranja_suite =
  {
    name = "dolzina_kodiranja";
    fn = uncurry_f_2 dolzina_kodiranja;
    cases =
      [
        {
          input =
            ( [ ('a', 5); ('b', 2); ('d', 1); ('k', 1); ('r', 2) ],
              primer_drevesa );
          expected_output = Ok 23;
          description = "Besedilo abrakadabra";
        };
        {
          input =
            ([ ('i', 4); ('m', 1); ('p', 2); ('s', 4) ], drevo_mississippi);
          expected_output = Ok 21;
          description = "Besedilo mississippi";
        };
        {
          input =
            ( [ ('a', 5); ('b', 2); ('d', 1); ('k', 1); ('r', 2) ],
              Vozlisce
                ( Vozlisce (List 'a', List 'b'),
                  Vozlisce (List 'd', Vozlisce (List 'k', List 'r')) ) );
          expected_output = Ok 25;
          description = "Besedilo abrakadabra z drugim drevesom";
        };
        {
          input = ([], primer_drevesa);
          expected_output = Ok 0;
          description = "Prazno besedilo";
        };
        {
          input = (frekvence_danes, drevo_danes);
          expected_output = Ok 49;
          description = "Stavek s presledki";
        };
      ];
  }

let stisni_suite =
  {
    name = "stisni";
    fn = stisni;
    cases =
      [
        {
          input = "ab";
          expected_output = Ok (Vozlisce (List 'a', List 'b'), [ O; I ]);
          description = "Besedilo ab";
        };
        {
          input = "abrakadabra";
          expected_output = Ok (primer_drevesa, biti_abrakadabra);
          description = "Besedilo abrakadabra";
        };
        {
          input = "mississippi";
          expected_output = Ok (drevo_mississippi, biti_mississippi);
          description = "Besedilo mississippi";
        };
        {
          input = besedilo_danes;
          expected_output = Ok (drevo_danes, biti_danes);
          description = "Stavek s presledki";
        };
      ];
  }

let razsiri_suite =
  {
    name = "razsiri";
    fn = razsiri;
    cases =
      [
        {
          input = (Vozlisce (List 'a', List 'b'), [ O; I ]);
          expected_output = Ok "ab";
          description = "Besedilo ab";
        };
        {
          input = (primer_drevesa, biti_abrakadabra);
          expected_output = Ok "abrakadabra";
          description = "Besedilo abrakadabra";
        };
        {
          input = (drevo_mississippi, biti_mississippi);
          expected_output = Ok "mississippi";
          description = "Besedilo mississippi";
        };
        {
          input = (drevo_danes, biti_danes);
          expected_output = Ok besedilo_danes;
          description = "Stavek s presledki";
        };
      ];
  }

let razmerje_stiskanja_suite =
  {
    name = "razmerje_stiskanja";
    fn = razmerje_stiskanja;
    cases =
      [
        {
          input = "ab";
          expected_output = Ok 0.125;
          description = "Besedilo ab";
        };
        {
          input = "aabb";
          expected_output = Ok 0.125;
          description = "Besedilo aabb";
        };
        {
          input = "aaaabbcd";
          expected_output = Ok 0.21875;
          description = "Besedilo aaaabbcd";
        };
        {
          input = "abcdabcdabcdabcd";
          expected_output = Ok 0.25;
          description = "Štirje enako pogosti znaki";
        };
        {
          input = besedilo_danes;
          expected_output = Ok 0.3828125;
          description = "Stavek s presledki";
        };
      ];
  }
;;

run_suite v_niz_bitov_suite true;;
run_suite iz_niza_bitov_suite true;;
run_suite tabela_kod_suite true;;
run_suite kodiraj_suite true;;
run_suite dekodiraj_suite true;;
run_suite frekvence_suite true;;
run_suite rang_suite true;;
run_suite je_levicarska_kopica_suite true;;
run_suite sestavi_suite true;;
run_suite zlij_suite true;;
run_suite uredi_s_seznamom_suite true;;
run_suite uredi_s_kopico_suite true;;
run_suite zlij_seznamov_suite true;;
run_suite zlij_kopic_suite true;;
run_suite huffman_s_seznamom_suite true;;
run_suite huffman_s_kopico_suite true;;
run_suite dolzina_kodiranja_suite true;;
run_suite stisni_suite true;;
run_suite razsiri_suite true;;
run_suite razmerje_stiskanja_suite true
