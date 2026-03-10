(** Connections relate channels to blocks or other channels. *)

type child_type =
  | Block
  | Channel

let child_type_to_string = function
  | Block -> "block"
  | Channel -> "channel"

type t = {
  id : string;
  user_id : string;
  parent_channel_id : string;
  child_id : string;
  child_type : string;
  position : int;
}

let insert =
  [%rapper
    execute
      {sql|
        INSERT INTO connections (id, user_id, parent_channel_id, child_id, child_type, position)
        VALUES (%string{id}, %string{user_id}, %string{parent_channel_id},
                %string{child_id}, %string{child_type}, %int{position})
      |sql}]

let list_for_channel =
  [%rapper
    get_many
      {sql|
        SELECT @string{id}, @string{user_id}, @string{parent_channel_id},
               @string{child_id}, @string{child_type}, @int{position}
        FROM connections
        WHERE parent_channel_id = %string{parent_channel_id}
          AND user_id = %string{user_id}
        ORDER BY position ASC
      |sql}
      record_out]

let delete =
  [%rapper
    execute
      {sql|
        DELETE FROM connections
        WHERE id = %string{id} AND user_id = %string{user_id}
      |sql}]

let delete_for_block =
  [%rapper
    execute
      {sql|
        DELETE FROM connections
        WHERE child_id = %string{child_id} AND user_id = %string{user_id}
      |sql}]

