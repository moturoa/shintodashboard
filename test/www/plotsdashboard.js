Shiny.addCustomMessageHandler("plotsdashboard", function(data){
  
  let plots = $("#" + data.placeholder, " > div").map(function() { 
    return this.id; 
  }).get(); 
  
  Shiny.onInputChange(data.shiny_id, plots);
  
});
