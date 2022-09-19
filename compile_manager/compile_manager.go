package compilemanager

import (
	"proto/shared"
)

type Manager struct {
	Path     string
	CleanSrc bool
}

func NewManager(path string, clean bool) *Manager {
	man := &Manager{
		Path:     shared.Get_abs_path("CompileManager", path),
		CleanSrc: clean,
	}
	return man
}

func (m *Manager) Compile() {
	project_org := NewProjectManager(m.Path, m.CleanSrc)
	project_org.BuildProject()
}
