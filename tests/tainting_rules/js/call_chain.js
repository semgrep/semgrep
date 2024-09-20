function foo($scope) {
  //ruleid: detect-angular-element-methods
  var now = angular.element($scope.input).html();
};

