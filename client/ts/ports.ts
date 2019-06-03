import * as blockstack from 'blockstack'
import { App } from './application'

type Project = {
  uuid: string,
  address: string,
  description: string,
  featuredImageUrl: string,
  goal: number,
  title: string
}

function authenticate (): void {
  blockstack
    .redirectToSignIn(
      // 'https://blocos.app',
      // 'https://blocos.app/manifest.json',
      'http://localhost:8000',
      'http://localhost:8000/manifest.json',
      ['publish_data']
    )
}

function signOut (): void {
  blockstack.signUserOut('/')
}

export function handleAuthentication (app: App): void {
  app.ports.authenticate.subscribe(authenticate)
  app.ports.signOut.subscribe(signOut)

  if (blockstack.isUserSignedIn()) {
    // TODO:replace loadUserData as it is now deprecated
    const user = blockstack.loadUserData()
    if (user) {
      app.ports.authenticated.send({
        username: user.username,
        name: 'name',
        profilePicture: 'https://profile.picture'
      })
      const profile = new blockstack.Person(user.profile)
      fetchSavedFiles(app)
    }
  }

  if (blockstack.isSignInPending()) {
    blockstack
      .handlePendingSignIn()
      .then(user => {
        if (user) {
          app.ports.authenticated.send({
            username: user.username,
            name: 'name',
            profilePicture: 'https://profile.picture'
          })
          fetchSavedFiles(app)
        }
      })
      .catch()
  }
}

function parseFile (fileContent: string): Project | null {
  if (!fileContent) {
    return null
  }

  try {
    const parsedFile = JSON.parse(fileContent)
    if (!parsedFile.uuid) {
      return null
    }

    return {
      uuid: parsedFile.uuid,
      address: parsedFile.address || '',
      description: parsedFile.description || '',
      featuredImageUrl: parsedFile.featuredImageUrl || '',
      goal: typeof parsedFile.goal === 'number' ? parsedFile.goal : 0,
      title: parsedFile.title || ''
    }
  } catch (error) {
    console.error(error)
  }
}

function fetchFile (app: App): (arg0: string) => boolean {
  return (filePath: string) => {
    blockstack
      .getFile(filePath, null)
      .then(savedFile => {
        if (typeof savedFile === 'string') {
          const parsedFile = parseFile(savedFile)
          if (parsedFile) {
            app.ports.fileSaved.send(parsedFile)
          }
        }
      })
      .catch(console.error)
    return true
  }
}

function fetchSavedFiles (app: App): void {
  blockstack
    .listFiles(fetchFile(app))
    .catch(console.error)
}

function subscribeToPutFile (app: App, fileSaved: (arg0: Project) => void) {
  app.ports.putFile.subscribe(project => {
    const fileName = project.uuid
    const fileContent = JSON.stringify(project)
    blockstack
      .putFile(fileName + '.json', fileContent)
      .then(() => fileSaved(project))
      .catch(console.error)
  })
}

function subscribeToDeleteFile (app: App, fileDeleted: (data: null) => void) {
  app.ports.deleteFile.subscribe(project => {
    const fileName = project.uuid + '.json'
    // TODO: replace empty putfile for deleteFile
    blockstack
      .putFile(fileName, '')
      .then(() => fileDeleted(null))
      .catch(console.error)
  })
}

export function handleFiles (app: App): void {
  subscribeToPutFile(app, app.ports.fileSaved.send)
  subscribeToDeleteFile(app, app.ports.fileDeleted.send)
}
