let uploadedFiles = {}; // Store uploaded files

export const flags = ({ env }) => {
  return {
    accessToken: JSON.parse(window.localStorage.token || null),
  };
};

export const onReady = ({ env, app }) => {
  // Called after our Elm application starts
  if (app.ports && app.ports.sendToLocalStorage) {
    app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
      window.localStorage[key] = JSON.stringify(value);
    });
  }

  // Store files from Elm
  if (app.ports && app.ports.storeFile) {
    app.ports.storeFile.subscribe(({ name, file }) => {
      console.log("Storing file:", name, file);
      uploadedFiles[name] = file;
    });
  }

  // PDF download handler
  if (app.ports && app.ports.sendPdfRequest) {
    app.ports.sendPdfRequest.subscribe(({ url, letterData, files }) => {
      fetch(url, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          ...letterData,
          photo: files.photo?.name ? uploadedFiles[files.photo.name] : null,
          background: files.background?.name
            ? uploadedFiles[files.background.name]
            : null,
        }),
      })
        .then((res) => res.blob())
        .then((blob) => {
          const blobUrl = URL.createObjectURL(blob);

          // Send URL back to Elm
          if (app.ports.receivePdfUrl) {
            app.ports.receivePdfUrl.send(blobUrl);
          }

          // Trigger download
          const a = document.createElement("a");
          a.href = blobUrl;
          a.download = "nota-falecimento.pdf";
          a.click();

          // Cleanup
          URL.revokeObjectURL(blobUrl);
        });
    });
  }
};
