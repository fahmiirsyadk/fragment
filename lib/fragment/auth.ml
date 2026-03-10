(** Authentication payload type and password hashing utilities. *)

type payload = {
  email: string;
  password_raw: string; [@key "password"]
} [@@deriving yojson]

(** Hash a plaintext password. Returns the hash string to store. *)
let hash plaintext =
  Bcrypt.hash plaintext |> Bcrypt.string_of_hash

(** Verify a plaintext password against a stored hash string.
    Returns false on any error (malformed hash, wrong cost, etc.) rather than
    raising — Bcrypt.verify itself can throw Bcrypt_error on corrupt input. *)
let verify plaintext hash_string =
  match Bcrypt.hash_of_string hash_string with
  | exception _ -> false
  | hash ->
    (match Bcrypt.verify plaintext hash with
    | result -> result
    | exception _ -> false)
