let customplotorder = function(){

	let divs = document.querySelectorAll(".customplot");

	let ids = [];

	for(let i = 0; i < divs.length; i++){

		ids[i] = divs[i].id;

	};

	Shiny.onInputChange("controls-customplotids", ids);
	
};

