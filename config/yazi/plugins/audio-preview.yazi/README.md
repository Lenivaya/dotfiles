# audio-preview.yazi

A plugin for [yazi](https://github.com/sxyazi/yazi) to preview soundfiles as spectrogram
using sox

## Requirements

- `sox`

## Installation

```sh
ya pack -a 'gesellkammer/audio-preview'
```

## Usage

Add this to your `yazi.toml`:

```toml
[plugin]
prepend_previewers = [
  { mime = "audio/*", run = "audio-preview" },
]

prepend_preloaders = [
  { mime = "audio/*", run = "audio-preview" },
]
```
