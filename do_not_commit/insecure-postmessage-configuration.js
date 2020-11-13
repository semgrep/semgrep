let data={pName : "Bob", pAge: "35"};
var popup = window.open(/* popup details */);

//ruleid:wildcard-postmessage-configuration
popup.postMessage(data, '*');
//ruleid:wildcard-postmessage-configuration
popup.postMessage( JSON.stringify( data ), '*' );

//postMessage Safe Usage
popup.postMessage("hello there!", "http://domain.tld");    
popup.postMessage( JSON.stringify( data ), 'semgrep.dev/editor');

//ruleid:insufficient-postmessage-origin-validation
window.addEventListener("message", receiveMessage, false);


//addEventListener Safe Usage (Origin Checked)
const globalRegex = RegExp('/^http://www.examplesender.com$/', 'g');
window.addEventListener("message", function(message){
    if(globalRegex.test(message.origin)){
         console.log(message.data);
   }
});

//addEventListener Safe Usage (Origin Checked)
window.addEventListener("message", function(message){
if (event.origin !== "http://example.com") {
    return;
}
});

//addEventListener Safe Usage (Origin Checked)
window.addEventListener("message", function(message){
if (event.origin == "http://example.com") {
    alert("Works");
}
});
