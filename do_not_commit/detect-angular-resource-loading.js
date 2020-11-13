var app = angular.module('MyApp', []).config(function ($sceDelegateProvider) {
    // ruleid: detect-angular-resource-loading
    $sceDelegateProvider.resourceUrlWhitelist([ '**' ]);
    
    // ruleid: detect-angular-resource-loading
    $sceDelegateProvider.resourceUrlWhitelist(['http://semgrep.dev', '**']);

    // Only one site is whitelisted, assumed to be safe
    $sceDelegateProvider.resourceUrlWhitelist(['http://semgrep.dev']);
    
});
 app.controller('myCtrl', function($scope) {
 
 $scope.userInput = 'foo';
     $scope.sayHello = function() {
	  $scope.html = "Hello <b>" + $scope.userInput + "</b>!"

    };
 
 });
