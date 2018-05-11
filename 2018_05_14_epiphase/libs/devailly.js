remark.macros.img_scale = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};

remark.macros.my_color = function (color) {
  var content = this;
  return '<p style="color:' + color + '">' + content + '</p>';
};

