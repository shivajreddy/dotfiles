$ErrorActionPreference = "Stop"

$ClaudeCredsFile = "$env:USERPROFILE\.claude\.credentials.json"
$OpencodeAuthFile = "$env:USERPROFILE\.local\share\opencode\auth.json"

Write-Host "Reading Claude Code credentials..."
$credsContent = Get-Content $ClaudeCredsFile -Raw | ConvertFrom-Json

$access  = $credsContent.claudeAiOauth.accessToken
$refresh = $credsContent.claudeAiOauth.refreshToken
$expires = $credsContent.claudeAiOauth.expiresAt

if (-not $access -or $access -eq "null") {
    Write-Error "Could not extract credentials from $ClaudeCredsFile"
    exit 1
}

Write-Host "Updating opencode auth.json..."
$auth = Get-Content $OpencodeAuthFile -Raw | ConvertFrom-Json

$auth.anthropic = [PSCustomObject]@{
    type    = "oauth"
    access  = $access
    refresh = $refresh
    expires = $expires
}

$auth | ConvertTo-Json -Depth 10 | Set-Content $OpencodeAuthFile -Encoding UTF8

Write-Host "Done."
