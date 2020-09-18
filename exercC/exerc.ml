type regexp =
  | V  
  | E
  | C of char
  | U of regexp * regexp 
  | P of regexp * regexp 
  | S of regexp
 
module Parser_regexp = struct

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | RPAREN
    | LPAREN
    | EPS
    | EOF
    | EMP
    | CONC
    | CHAR of (
      (char)
  )
    | AST
    | ALT
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState12
  | MenhirState7
  | MenhirState6
  | MenhirState1
  | MenhirState0


let rec _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
                                ( P (e1, e2) )
        in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)) : 'freshtv48)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
                                ( U (e1, e2) )
        in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_atom = 
                                ( e )
            in
            _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (le : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
      (regexp)
            ) = 
                                ( le )
            in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
      (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
      (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
      (regexp)
            )) : (
      (regexp)
            )) = _v in
            (Obj.magic _1 : 'freshtv62)) : 'freshtv64)) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv75)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState12 | MenhirState6 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ALT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv32)
        | CONC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv34)
        | EOF | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_term)) = _menhir_stack in
            let _v : 'tv_expr = 
                                ( e )
            in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_factor)), _, (e2 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_term = 
                                ( P (e1, e2) )
        in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv42)) : 'freshtv44)

and _menhir_goto_factor : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_factor -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ALT | CONC | EOF | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_factor)) = _menhir_stack in
        let _v : 'tv_term = 
                                ( e )
        in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv30)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_factor = 
                                ( S e )
        in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)) : 'freshtv22)
    | ALT | CHAR _ | CONC | EMP | EOF | EPS | LPAREN | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_factor = 
                                ( e )
        in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv24)) : 'freshtv26)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv18)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom = 
                                ( E )
    in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom = 
                                ( V )
    in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
      (char)
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
      (char)
    )) : (
      (char)
    )) = _v in
    ((let _v : 'tv_atom = 
                                ( C c )
    in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and regexpr : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
      (regexp)
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

  



end

module Lexer_regexp = struct

  open Parser_regexp

  exception Error of string


  let __ocaml_lex_tables = {
    Lexing.lex_base =
    "\000\000\245\255\246\255\247\255\248\255\249\255\250\255\251\255\
      \252\255\253\255\254\255\255\255";
    Lexing.lex_backtrk =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255";
    Lexing.lex_default =
    "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000";
    Lexing.lex_trans =
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \004\000\003\000\007\000\009\000\000\000\000\000\008\000\000\000\
      \006\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \002\000";
    Lexing.lex_check =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
      \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000";
    Lexing.lex_base_code =
    "";
    Lexing.lex_backtrk_code =
    "";
    Lexing.lex_default_code =
    "";
    Lexing.lex_trans_code =
    "";
    Lexing.lex_check_code =
    "";
    Lexing.lex_code =
    "";
  }

  let rec tokenize lexbuf =
    __ocaml_lex_tokenize_rec lexbuf 0
  and __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state =
    match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
        | 0 ->
                                        ( tokenize lexbuf )

    | 1 ->
  let
                          s
  = Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
                                        ( CHAR s )

    | 2 ->
                                        ( ALT )

    | 3 ->
                                        ( CONC )

    | 4 ->
                                        ( AST )

    | 5 ->
                                        ( EMP )

    | 6 ->
                                        ( EPS )

    | 7 ->
                                        ( LPAREN )

    | 8 ->
                                        ( RPAREN )

    | 9 ->
                                        ( EOF )

    | 10 ->
        ( raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) )

    | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
        __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state

  ;;
end

(* --------------------------------- fim lexing/parsing code ----------------------------------------------------- *)

open Parser_regexp


(* função principal de leitura de uma expressão regular (a partir de uma string) *)
let regexp st =
  let linebuf = Lexing.from_string st in
  try regexpr Lexer_regexp.tokenize linebuf
  with _ -> failwith "regexp: input problem"


(* **************************************************************************************************************** *)
(* ********************************************   Começar aqui **************************************************** *)


(* exemplo de código para ilustrar o uso da função regexp e o tipo regexp *)
let rec string_of_regexp s =
  match s with
  | V       -> "0"
  | E       -> "1"
  | C  c    -> String.make 1 c    
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"

(*
  V -> 
  E -> Estado E ??
  C c -> Transição c
  U f g -> União entre f e g ( + )
  P f g -> Produto entre f e g ( . )
  S s -> Zero or more ( * )
*)

type state = {
  isEnd : bool;
  mutable transitions : (string * state) list;
  mutable epsilonTransitions : state list
}

let createState isFinal =
  {isEnd = isFinal; transitions = []; epsilonTransitions = []}

let initialState = createState false


let getLetter (a, _) = a
let getSt (_, a) = a
let rec printTransLst from lst =
  match lst with
    | a::b -> Printf.printf "\n s%d -%s> s%d " from.id (getLetter a) (getSt a).id; printTransLst from b
    | [] -> ()
let rec printEpsilonTransLst from lst =
  match lst with
    | a::b -> Printf.printf "\n s%d -Ep> s%d " from.id a.id; printEpsilonTransLst from b
    | [] -> ()
let rec printStateList lst = 
  match lst with
    | a::b -> printTransLst a a.transitions; printEpsilonTransLst a a.epsilonTransitions; printStateList b
    | [] -> ()

let rec printAutomaton ist =
  printTrans ist ist.transitions;
  printEpTrans ist ist.epsilonTransitions
and printTrans fromSt lst =
  match lst with
    | a::b -> (Printf.printf "\n s%d -%s> s%d " fromSt.id (getLetter a) (getSt a).id; printTrans fromSt b; printAutomaton (getSt a))
    | [] -> ()
and printEpTrans fromSt lst =
  match lst with
    | a::b -> (Printf.printf "\n s%d -EP> s%d " fromSt.id a.id; printEpTrans fromSt b; printAutomaton a)
    | [] -> ()

    let rec createAutomaton regex currentState finalState =
      match regex with
        | V       -> "0"
        | E       -> "1"
        | C  c    -> String.make 1 c    
        | U (f,g) -> union f g currentState finalState
        | P (f,g) -> product f g currentState finalState
        | S s     -> zeroOrMore s currentState finalState
    
    and union f g currentState finalState =
      let q1 = createState false in
        let q11 = createState false in
          let q2 = createState false in
            let q22 = createState false in
              (
                currentState.epsilonTransitions <- currentState.epsilonTransitions@[q1];
                q1.transitions <- q1.transitions@[((createAutomaton f q1 q11), q11)];
                q11.epsilonTransitions <- q11.epsilonTransitions@[finalState];
    
                currentState.epsilonTransitions <- currentState.epsilonTransitions@[q2];
                q2.transitions <- q2.transitions@[((createAutomaton g q2 q22), q22)];
                q22.epsilonTransitions <- q22.epsilonTransitions@[finalState]; "(1?"^(string_of_regexp f)^":"^(string_of_regexp g)^")"
              )
    
    and product f g currentState finalState =
      let newState = createState false in
        (currentState.transitions <- currentState.transitions@[((createAutomaton f currentState newState), newState)];
        newState.transitions <- newState.transitions@[((createAutomaton g newState finalState), finalState)]; "(2?"^(string_of_regexp f)^":"^(string_of_regexp g)^")")
    
    and zeroOrMore s currentState finalState =
      let q1 = createState false in
        let q2 = createState false in
        (
          currentState.epsilonTransitions <- currentState.epsilonTransitions@[q1];
          currentState.epsilonTransitions <- currentState.epsilonTransitions@[finalState];
          q2.epsilonTransitions <- q2.epsilonTransitions@[finalState];
          q2.epsilonTransitions <- q2.epsilonTransitions@[q1];
          q1.transitions <- q1.transitions@[((createAutomaton s q1 q2), q2)]; "(3?"^(string_of_regexp s)^")"
        )

        
let rec createAutomaton regex currentState finalState =
  match regex with
    | V       -> "0"
    | E       -> "1"
    | C  c    -> String.make 1 c    
    | U (f,g) -> union f g currentState finalState
    | P (f,g) -> product f g currentState finalState
    | S s     -> zeroOrMore s currentState finalState

and union f g currentState finalState =
  (currentState.transitions <- currentState.transitions@[((createAutomaton f currentState finalState), finalState)];
  currentState.transitions <-  currentState.transitions@[((createAutomaton g currentState finalState), finalState)]; "")

and product f g currentState finalState =
  let newState = createState false in
    (currentState.transitions <- currentState.transitions@[((createAutomaton f currentState newState), newState)];
    newState.transitions <- newState.transitions@[((createAutomaton g newState finalState), finalState)]; "")

and zeroOrMore s currentState finalState =
  let q1 = createState false in
    let q2 = createState false in
    (
      currentState.epsilonTransitions <- currentState.epsilonTransitions@[q1];
      currentState.epsilonTransitions <- currentState.epsilonTransitions@[finalState];
      q2.epsilonTransitions <- q2.epsilonTransitions@[finalState];
      q2.epsilonTransitions <- q2.epsilonTransitions@[q1];
      createAutomaton s q1 q2
    )

(*
  States que conseguiu alcançar até o momento
  States que está a analisar
*)
let atualStates = ref [initialState]

(*
  Função que verifica tupulo
*)
let checkLetter (a, _) letter = 
  if a = String.make 1 letter then true else false
let getState (_, a) = a

(*
  Função que precorre lista de transitions
  -> Devolve lista com novos states
*)
let rec checkTransitions lst checkForChar return =
  match lst with
    | [] -> return
    | a::b -> if checkLetter a checkForChar
      then (if List.mem a !visited
        then checkTransitions b checkForChar return
        else (visited := !visited@[(getState a)]; checkTransitions b checkForChar return@[(getState a)]))
      else checkTransitions b checkForChar return

(*
  Função que precorre listas de epsilonTransitions
  -> Devolve lista com novos states
*)
let visited = ref []

let rec getAllEpsilonStates epsilonTransitionList return =
  match epsilonTransitionList with
    | a::b -> if List.mem a !visited
      then getAllEpsilonStates b return
      else (visited := !visited@[a]; getAllEpsilonStates b (return@[a]@(getAllEpsilonStates a.epsilonTransitions [])))
    | [] -> return

(*
  Check if exists a final State between the current states
*)
let rec checkForFinal atualLst =
  match atualLst with
    | a::b -> if a.isEnd
      then true
      else checkForFinal b
    | [] -> false

(*
  Check atual States
  -> Devolve lista de novos States
*)
let rec checkAtualStates currentList atualChar return =
  match currentList with 
    | a::b -> let newStates = (checkTransitions (a.transitions) atualChar [])@[initialState] in
      let newStates1 = getAllEpsilonStates newStates [] in
          checkAtualStates b atualChar (return@newStates@newStates1)
    | [] -> return

(*
  Função que precorre lista de estado atuais
  -> Substitui lista existente
*)
let rec check word pos =
  let () = (Printf.printf "\n\n pos %d: " pos; printStateList (!atualStates) 0) in
  let () = visited := [] in
    if pos = String.length word
    then "NO"
    else
      let newAtualStates = checkAtualStates (!atualStates) (String.get word pos) [] in
        (atualStates := newAtualStates;
        if checkForFinal !atualStates
        then "YES"
        else check word (pos+1))

(*

*)
let () =
  let r = regexp Sys.argv.(1) in
    let () = print_string "input: " in
      let () = print_endline (string_of_regexp r) in
        let _ = createAutomaton r initialState (createState true) in
          let word = Sys.argv.(2) in
            let output = check word 0 in
              print_endline output
