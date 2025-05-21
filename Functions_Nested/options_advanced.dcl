AdvancedOptions : dialog {
  label = "Choose Action and Parameters";
  
  : popup_list {
    key = "main_action";
    label = "Action:";
    list = "Add Circle\nAdd Text";
  }

  : edit_box {
    key = "param1";
    label = "Radius / Text:";
    value = "";
  }

  : button {
    key = "accept";
    label = "OK";
    is_default = true;
  }
  : button {
    key = "cancel";
    label = "Cancel";
    is_cancel = true;
  }
}