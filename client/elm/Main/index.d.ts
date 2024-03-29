// WARNING: Do not manually modify this file. It was generated using:
// https://github.com/dillonkearns/elm-typescript-interop
// Type definitions for Elm ports

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        authenticate: {
          subscribe(callback: (data: null) => void): void
        }
        signOut: {
          subscribe(callback: (data: null) => void): void
        }
        putFile: {
          subscribe(callback: (data: { address: string | null; description: string; featuredImageUrl: string; goal: number; uuid: string; title: string }) => void): void
        }
        deleteFile: {
          subscribe(callback: (data: { address: string | null; description: string; featuredImageUrl: string; goal: number; uuid: string; title: string }) => void): void
        }
        checkAuthentication: {
          subscribe(callback: (data: null) => void): void
        }
        authenticated: {
          send(data: unknown): void
        }
        fileSaved: {
          send(data: { address: string | null; description: string; featuredImageUrl: string; goal: number; uuid: string; title: string }): void
        }
        fileDeleted: {
          send(data: null): void
        }
      };
    }
    export function init(options: {
      node?: HTMLElement | null;
      flags: [number, number[]];
    }): Elm.Main.App;
  }
}