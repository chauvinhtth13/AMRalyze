$(document).on('shiny:connected', function () {
  var vsInputId = 'AB_cols';
  var lastSelectedIndex = -1;
  var virtualSelectElement = document.getElementById(vsInputId);

  if (virtualSelectElement && virtualSelectElement.virtualSelect) {
    $(virtualSelectElement).on('click', '.vscomp-option', function (e) {
      var clickedOption = $(this);
      var currentIndex = clickedOption.data('index');

      if (e.shiftKey && lastSelectedIndex !== -1) {
        e.preventDefault(); // Prevent toggle behavior
        var start = Math.min(lastSelectedIndex, currentIndex);
        var end = Math.max(lastSelectedIndex, currentIndex);
        var optionsToSelect = [];

        $('.vscomp-option').each(function () {
          var idx = $(this).data('index');
          if (idx >= start && idx <= end) {
            optionsToSelect.push($(this).data('value'));
          }
        });

        console.log('Selecting range:', optionsToSelect);
        virtualSelectElement.virtualSelect.setValue(optionsToSelect);
      } else {
        lastSelectedIndex = currentIndex;
      }
    });
  } else {
    console.warn('VirtualSelect not initialized or missing for:', vsInputId);
  }
});
