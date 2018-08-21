

$(document).on('click', '.itemEC', function () {
  Shiny.onInputChange('last_enc',this.id);
  var bisabuelo = this.parentNode.parentNode.parentNode
  var button = bisabuelo.querySelector('button')
  var isActive = document.querySelector('.butTemas.activEnc')
  if (isActive) {
    isActive.classList.remove('activEnc')
  }
  button.classList.add('activEnc')
});




$(document).on('click', '.barType', function () {
  Shiny.onInputChange('last_graf',this.id);
});
