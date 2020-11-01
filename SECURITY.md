# Security Policy

## Supported Versions

This project is under active development and we do our best to support the latest versions.

| Version | Supported          |
| ------- | ------------------ |
| latest   | :white_check_mark: |

## Reporting a Vulnerability

Please email security@r2c.dev with any security issues in Semgrep. We take all reports seriously.
FirebaseCustomRemoteModel remoteModel =
      new FirebaseCustomRemoteModel.Builder("your_model").build();
FirebaseModelDownloadConditions conditions = new FirebaseModelDownloadConditions.Builder()
        .requireWifi()
        .build();
FirebaseModelManager.getInstance().download(remoteModel, conditions)
        .addOnSuccessListener(new OnSuccessListener<Void>() {
            @Override
            public void onSuccess(Void v) {
              // Download complete. Depending on your app, you could enable
              // the ML feature, or switch from the local model to the remote
              // model, etc.
            }
        });
