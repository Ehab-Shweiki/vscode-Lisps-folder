OptionsDialog : dialog {
  label = "Choose an Option";
  : popup_list {
    key = "option_list";
    label = "Select:";
    list = "Option1\nOption2\nOption3";
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