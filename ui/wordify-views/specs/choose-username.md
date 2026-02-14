# Choose Username View

A view that prompts the user to choose a unique username. This is displayed after signup when the user has not yet set a username. Once chosen, the username cannot be changed.

## Prompt

The view should display the following text to the user above the form:

> **Choose your username**
>
> Pick a username that will be displayed publicly whenever you play games, send messages, or appear on scoreboards. Choose carefully — you won't be able to change it later.

## Configuration

The view accepts the following configuration:

* `redirectUrl` (string, required) — a relative path to navigate to after a username is successfully chosen (e.g. `"/"`)

## Form

The form consists of a single PrimeVue `InputText` field for the username and a `Button` to submit.

### Username Input

* Should use PrimeVue's `InputText` component
* Placeholder text: `"Enter a username"`
* The submit button label should be `"Claim Username"`
* The submit button should be disabled while the input is empty or while a submission is in flight

### Validation (client-side)

* The username must be between 3 and 20 characters
* The username may only contain letters, numbers, hyphens, and underscores (regex: `^[a-zA-Z0-9_-]+$`)
* Validation errors should be displayed inline below the input field using PrimeVue's `Message` component with `severity="error"`

## API

### Submit Endpoint

When the user clicks "Claim Username", the view sends:

```
POST {window.location.origin}/api/username
Content-Type: application/json
```

**Request payload:**

```json
{
  "username": "chosen-username"
}
```

### Success Response

**Status:** `200 OK`

No response body is required. On receiving a 200, the client should navigate to the configured `redirectUrl`.

### Username Taken Response

**Status:** `409 Conflict`

```json
{
  "error": "username_taken",
  "message": "This username is already taken. Please choose another."
}
```

On receiving a 409, the view should display the `message` field from the response body inline below the input field using PrimeVue's `Message` component with `severity="error"` (same positioning as client-side validation errors).

### Other Errors

For any other non-200 response, display a generic error message inline below the input:

> Something went wrong. Please try again.

## Responsiveness Concerns

This view should display well on mobile and desktop. It should follow the same centered card layout pattern used by `LoginView.vue` — a full-viewport-height page with a centered `Card` component, max width `max-w-md`.

## Redirect on Success

After a `200 OK` response, the client should navigate to the `redirectUrl` relative path provided in the configuration using `window.location.href` assignment.

## Library Export

This should be exported in the `lib` folder in a `choose-username.ts` file following the same pattern as `login.ts`.

The mounting function should be `createChooseUsername(element, options)`.

**Parameters:**
- `element` — CSS selector string or DOM element
- `options` — `ChooseUsernameOptions` object:
  - `redirectUrl` (string, required) — relative path to navigate to on success (e.g. `"/"`)

**Returns:** `ChooseUsernameInstance` object with:
- `app` — The Vue app instance
- `unmount()` — Cleanup function

The top level `src/lib.ts` file should re-export it.

**Example:**

```typescript
import { createChooseUsername } from 'wordify-views'

const chooseUsername = createChooseUsername('#choose-username-container', {
  redirectUrl: '/'
})

// Later: cleanup
chooseUsername.unmount()
```
