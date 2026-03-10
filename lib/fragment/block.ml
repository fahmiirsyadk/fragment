(** Block record and queries. *)

type kind =
  | Link
  | Image
  | Text
  | Pdf

let kind_to_string = function
  | Link -> "link"
  | Image -> "image"
  | Text -> "text"
  | Pdf -> "pdf"

let kind_of_string = function
  | "link" -> Some Link
  | "image" -> Some Image
  | "text" -> Some Text
  | "pdf" -> Some Pdf
  | _ -> None

type t = {
  id : string;
  user_id : string;
  kind : string;
  content : string;
  title : string option;
  description : string option;
  image_url : string option;
  media_json : string option;
}

(* Insert a new block *)
let insert =
  [%rapper
    execute
      {sql|
        INSERT INTO blocks (id, user_id, kind, content, title, description, image_url, media_json)
        VALUES (%string{id}, %string{user_id}, %string{kind}, %string{content},
                %string?{title}, %string?{description}, %string?{image_url}, %string?{media_json})
      |sql}]

let list_by_user =
  [%rapper
    get_many
      {sql|
        SELECT @string{id}, @string{user_id}, @string{kind}, @string{content},
               @string?{title}, @string?{description}, @string?{image_url}, @string?{media_json}
        FROM blocks
        WHERE user_id = %string{user_id}
        ORDER BY created_at DESC
      |sql}
      record_out]

let get_by_id =
  [%rapper
    get_opt
      {sql|
        SELECT @string{id}, @string{user_id}, @string{kind}, @string{content},
               @string?{title}, @string?{description}, @string?{image_url}, @string?{media_json}
        FROM blocks
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}
      record_out]

let update_metadata =
  [%rapper
    execute
      {sql|
        UPDATE blocks
        SET title = %string?{title}, description = %string?{description}
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}]

let delete =
  [%rapper
    execute
      {sql|
        DELETE FROM blocks
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}]

