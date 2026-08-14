import { Elm } from "./src/Main.elm"

const now = new Date(Date.now())

const [year, month, date] = [
  now.getFullYear(),
  now.getMonth() + 1, // getMonth is zero indexed
  now.getDate(),
]

const seed = parseInt(`${year}${month}${date}`, 10)
const savePrefix = "save-"
const key = `${savePrefix}${seed}`
const savedState = localStorage.getItem(key)

// We haven't saved any progress for today's word
// Clear any existing saved state as well
if (!savedState) {
  const keys = Object.keys(localStorage).filter((x) => x.startsWith(savePrefix))
  for (const key of keys) {
    localStorage.removeItem(key)
  }
}

const savedSettings = localStorage.getItem("settings")

const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {
    seed,
    save: savedState ? JSON.parse(savedState) : null,
    settings: savedSettings ? JSON.parse(savedSettings) : null,
  },
})

app.ports.saveHistory.subscribe((history) =>
  localStorage.setItem(key, JSON.stringify(history))
)

app.ports.shareResult.subscribe(async (result) => {
  await navigator.clipboard.writeText(result)
})

app.ports.persistSettings.subscribe((settings) =>
  localStorage.setItem("settings", JSON.stringify(settings))
)