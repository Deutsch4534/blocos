// Define jest global namespace
declare namespace NodeJS {
  export interface Global {
    crypto: any
  }
}

