// ruleid:dom-based-xss
document.write("<OPTION value=1>"+document.location.href.substring(document.location.href.indexOf("default=")+8)+"</OPTION>");

// ok
document.write("<OPTION value=2>English</OPTION>");