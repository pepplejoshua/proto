package compilemanager

import (
	"proto/shared"
)

type Manager struct {
	Path     string
	CleanSrc bool
	Generate bool
	ShowInfo bool
	CppFlags string
}

func NewManager(path, cpp_flags string, clean bool, generate_only bool, show_compile_info bool) *Manager {
	man := &Manager{
		Path:     shared.Get_abs_path("CompileManager", path),
		CleanSrc: clean,
		Generate: generate_only,
		ShowInfo: show_compile_info,
		CppFlags: cpp_flags,
	}
	return man
}

func (m *Manager) Compile() {
	project_org := NewProjectManager(m.Path, m.CppFlags, m.CleanSrc, m.Generate, m.ShowInfo)
	project_org.BuildProject()
}
