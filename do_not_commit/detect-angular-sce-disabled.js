var app = angular.module('MyApp', []).config(function ($sceProvider) {
    // ruleid: detect-angular-sce-disabled 
    $sceProvider.enabled(false);
});
 app.controller('myCtrl', function($scope) {
 
 $scope.userInput = 'foo';
     $scope.sayHello = function() {
	  $scope.html = "Hello <b>" + $scope.userInput + "</b>!"

    };
 
 });


  var app = angular.module('MyApp2', []).config(function ($sceProvider) {
    $sceProvider.enabled(true);
});
