/*
description: it gives the row id of a DT table
Used by table_btns function in mod_o1_plan
*/
function get_id(clicked_id) {
  Shiny.setInputValue("tasks-current_id",
                      clicked_id,
                      {priority: "event"});
}
