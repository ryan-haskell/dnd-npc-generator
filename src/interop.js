
// Called when the Elm app is ready!
export const onReady = ({ app }) => {

  if (app.ports.saveCharacter) {
    app.ports.saveCharacter.subscribe(({ id, character }) => {
      let characters = JSON.parse(localStorage.getItem('characters')) || {}
      characters[id] = character
      localStorage.setItem('characters', JSON.stringify(characters))
    })
  }

  if (app.ports.loadCharacters) {
    let characters = JSON.parse(localStorage.getItem('characters')) || {}
    app.ports.loadCharacters.send(characters)
  }
}