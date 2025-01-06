export  const flags = ({env}) => {
  return {
    accessToken: JSON.parse(window.localStorage.token || null)
  }
}

export const onReady = ({ env, app }) => {
  // Called after our Elm application starts
  if (app.ports && app.ports.sendToLocalStorage) {
    app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
      window.localStorage[key] = JSON.stringify(value)
    })
  }
}
