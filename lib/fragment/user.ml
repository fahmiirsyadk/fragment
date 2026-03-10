(** User record and database queries. *)

type t = {
  id: string;
  email: string;
  password_hash: string;
}

let insert =
  [%rapper
    execute
      {sql|
        INSERT INTO users (id, email, password_hash)
        VALUES (%string{id}, %string{email}, %string{password_hash})
      |sql}]

let get_by_email =
  [%rapper
    get_opt
      {sql|
        SELECT @string{id}, @string{email}, @string{password_hash}
        FROM users
        WHERE email = %string{email}
      |sql}
      record_out]
