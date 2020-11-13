var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {
    // ruleid: detect-angular-translateprovider-useStrategy-method
    $translateSanitization.useStrategy();
    var output = 'Hallo <b>{{name}}</b>';
    // ruleid:detect-angular-translateprovider-translations-method
    $translateProvider.translations('de', {output});
    // ruleid:detect-angular-translateprovider-translations-method
    $translateProvider.translations('de', {GREETING: 'Hallo <b>{{name}}</b>'});

});
