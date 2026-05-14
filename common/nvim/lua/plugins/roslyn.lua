return {
  "seblj/roslyn.nvim",
  ft = "cs",
  opts = {
    exe = {
      "dotnet",
      vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", "roslyn", "libexec", "Microsoft.CodeAnalysis.LanguageServer.dll"),
    },
    --NOTE: for a list of filetypes roslyn will attach to
    filewatching = true,
  },
}
