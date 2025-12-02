# Accepting Changes (Red/Green Diffs)

When you see red/green lines in VS Code or in `git diff`, those lines show what will be removed (red) or added (green). Here’s how to accept or apply them:

## With VS Code Source Control
1. Open the **Source Control** panel (Ctrl+Shift+G or the Git icon in the Activity Bar).
2. Click a file with changes to open the inline diff view.
3. In the gutter beside each change, use the action buttons:
   - **Accept Current Change** keeps the version already in your working file.
   - **Accept Incoming Change** keeps the version coming from the other side of a merge/rebase.
   - **Accept Both Changes** keeps both snippets.
   - **Undo** reverts that hunk to the pre-merge state.
4. After resolving all hunks, save the file. Stage it ("+" button or `git add <file>`).

## With the Command Line (no merge conflicts)
- To apply your local edits and stage them: `git add <file>` then `git commit -m "<message>"`.
- To discard a hunk interactively: `git add -p <file>` and choose **d** (discard) or **s** (split) as you review each hunk.
- To discard all local changes in a file: `git checkout -- <file>`.

## During a Merge or Rebase (CLI)
1. Open the conflicted file (markers `<<<<<<<`, `=======`, `>>>>>>>`).
2. Manually edit to keep the desired lines, removing the markers.
3. Run `git add <file>` to mark it resolved.
4. Continue the merge/rebase (`git merge --continue` or `git rebase --continue`).

## Quick defaults
- If you simply want to keep **your** edits over the incoming ones in a merge: `git checkout --theirs <file>` (incoming) or `git checkout --ours <file>` (yours), then `git add <file>`.
- To keep both sides, edit the file to include both sections, then `git add`.

After you accept the changes, always run any relevant checks and then commit. If you’re unsure, review the diff again with `git diff --staged` before finalizing.
