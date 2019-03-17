package main

import (
	"fmt"
	"os"
	"strings"

	git "gopkg.in/libgit2/git2go.v27"
)

const (
	GitAdded      = 'A'
	GitModified   = 'M'
	GitDeleted    = 'D'
	GitRenamed    = 'R'
	GitTypeChange = 'T'
	GitIgnored    = '!'
	GitUntracked  = '?'
)

var gitOrder = []byte{GitUntracked, GitAdded, GitModified, GitDeleted, GitRenamed, GitTypeChange}

func foreground(s, name string) string {
	return "%F{" + name + "}" + s + "%f"
}

func background(s, name string) string {
	return "%K{" + name + "}" + s + "%k"
}

func bold(s string) string {
	return "%B" + s + "%b"
}

func getStatus(status git.Status) (istatus byte, wstatus byte) {
	if status&git.StatusIndexNew == git.StatusIndexNew {
		istatus = GitAdded
	}
	if status&git.StatusIndexModified == git.StatusIndexModified {
		istatus = GitModified
	}
	if status&git.StatusIndexDeleted == git.StatusIndexDeleted {
		istatus = GitDeleted
	}
	if status&git.StatusIndexRenamed == git.StatusIndexRenamed {
		istatus = GitRenamed
	}
	if status&git.StatusIndexTypeChange == git.StatusIndexTypeChange {
		istatus = GitTypeChange
	}

	if status&git.StatusWtNew == git.StatusWtNew {
		if istatus == 0 {
			istatus = GitUntracked
		}
		wstatus = GitUntracked
	}
	if status&git.StatusWtModified == git.StatusWtModified {
		wstatus = GitModified
	}
	if status&git.StatusWtDeleted == git.StatusWtDeleted {
		wstatus = GitDeleted
	}
	if status&git.StatusWtRenamed == git.StatusWtRenamed {
		wstatus = GitRenamed
	}
	if status&git.StatusWtTypeChange == git.StatusWtTypeChange {
		wstatus = GitTypeChange
	}

	if status&git.StatusIgnored == git.StatusIgnored {
		istatus = GitIgnored
		wstatus = GitIgnored
	}

	return
}

type gitInfo struct {
	branch  string
	changes map[byte]int
}

func getGitInfo(pwd string, branch, status bool) (out gitInfo) {
	repo, err := git.OpenRepositoryExtended(pwd, 0, "")
	if err != nil {
		return
	}
	if branch {
		out.branch = gitBranch(repo)
	}
	if status {
		out.changes = gitStatus(repo)
	}
	return
}

func gitBranch(repo *git.Repository) string {
	ref, err := repo.Head()
	if err != nil {
		return ""
	}
	name, err := ref.Branch().Name()
	if err != nil {
		return ""
	}
	return name
}

func gitStatus(repo *git.Repository) (changes map[byte]int) {
	list, err := repo.StatusList(&git.StatusOptions{
		Show:  git.StatusShowIndexAndWorkdir,
		Flags: git.StatusOptIncludeUntracked,
	})
	if err != nil {
		return
	}

	count, _ := list.EntryCount()
	if count == 0 {
		return
	}
	changes = make(map[byte]int, 0)

	for i := 0; i < count; i++ {
		entry, _ := list.ByIndex(i)
		if entry.Status == git.StatusCurrent {
			continue
		}
		istatus, wstatus := getStatus(entry.Status)
		if istatus == GitIgnored {
			continue
		}
		if istatus == 0 && wstatus == 0 {
			continue
		}
		if istatus == wstatus {
			changes[istatus]++
		} else if istatus != 0 {
			changes[istatus]++
		} else if wstatus != 0 {
			changes[wstatus]++
		}
	}
	return
}

func formatPath(path string) (out string) {
	home := os.Getenv("HOME")
	if strings.Index(path, home) == 0 {
		out += "~"
		path = path[len(home):]
	}
	splitted := strings.Split(path, "/")
	for i := 0; i < len(splitted); i++ {
		s := splitted[i]
		if len(s) == 0 {
			continue
		}
		out += "/"
		if i < len(splitted)-1 {
			out += string(s[0])
		} else {
			out += s
		}
	}
	return
}

func main() {
	km := os.Getenv("KEYMAP")
	pwd, _ := os.Getwd()
	git := getGitInfo(pwd, true, false)
	var kmap string
	if km != "" {
		switch km {
		case "vicmd":
			kmap = "[N]"
		case "viins":
			fallthrough
		case "main":
			kmap = "[I]"
		}
	}
	var branchStr string
	if git.branch != "" {
		branchStr = "(" + foreground(git.branch, "yellow") + ")"
	}
	pathStr := foreground(formatPath(pwd), "red")
	fmt.Printf("%s%s%s> ", kmap, pathStr, branchStr)
}
