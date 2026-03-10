(** Channel record and queries. *)

type t = {
  id : string;
  user_id : string;
  title : string;
  description : string option;
}

(* Insert a new channel *)
let insert =
  [%rapper
    execute
      {sql|
        INSERT INTO channels (id, user_id, title, description)
        VALUES (%string{id}, %string{user_id}, %string{title}, %string?{description})
      |sql}]

(* List channels for a user, newest first *)
let list_by_user =
  [%rapper
    get_many
      {sql|
        SELECT @string{id}, @string{user_id}, @string{title}, @string?{description}
        FROM channels
        WHERE user_id = %string{user_id}
        ORDER BY created_at DESC
      |sql}
      record_out]

(* Fetch a single channel by id and user_id *)
let get_by_id =
  [%rapper
    get_opt
      {sql|
        SELECT @string{id}, @string{user_id}, @string{title}, @string?{description}
        FROM channels
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}
      record_out]

(* Delete a channel (cascades via foreign keys). *)
let delete =
  [%rapper
    execute
      {sql|
        DELETE FROM channels
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}]

let update =
  [%rapper
    execute
      {sql|
        UPDATE channels
        SET title = %string{title}, description = %string?{description}
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}]

