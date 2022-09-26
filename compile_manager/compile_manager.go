package compilemanager

import (
	"proto/shared"
)

type Manager struct {
	Path     string
	CleanSrc bool
	Generate bool
}

func NewManager(path string, clean bool, generate_only bool) *Manager {
	man := &Manager{
		Path:     shared.Get_abs_path("CompileManager", path),
		CleanSrc: clean,
		Generate: generate_only,
	}
	return man
}

func (m *Manager) Compile() {
	project_org := NewProjectManager(m.Path, m.CleanSrc, m.Generate)
	project_org.BuildProject()
}
