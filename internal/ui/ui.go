package ui

import (
	"fmt"
	"image/color"
	"strings"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/ebitenutil"
	"github.com/hajimehoshi/ebiten/v2/inpututil"
	"github.com/hajimehoshi/ebiten/v2/vector"
	"github.com/nevisdale/nestic/internal/nes"
)

// TODO: replace hardcode values with constants

// Tab - show debug info
// P - pause
// R - one step and stop

type UI struct {
	bus    *nes.Bus
	disasm map[uint16]string

	palette uint8
}

func New(bus *nes.Bus) *UI {
	return &UI{
		bus:    bus,
		disasm: bus.Disassemble(),
	}
}

func (ui *UI) Update() error {
	// if inpututil.IsKeyJustPressed(ebiten.KeyTab) {
	// 	ui.showDebugInfo = !ui.showDebugInfo
	// }

	if inpututil.IsKeyJustPressed(ebiten.KeyC) {
		ui.palette++
		if ui.palette > 7 {
			ui.palette = 0
		}
	}

	if inpututil.IsKeyJustPressed(ebiten.KeyP) {
		ui.bus.TooglePause()
	}

	if inpututil.IsKeyJustPressed(ebiten.KeyR) {
		ui.bus.OneStepAndStop()
	}

	ui.bus.Tic()
	return nil
}

func (ui *UI) Draw(screen *ebiten.Image) {

	info := ui.bus.DebugInfo()
	var infoStr strings.Builder
	fmt.Fprintf(&infoStr, " FPS: %0.0f\n", ebiten.ActualFPS())
	fmt.Fprintf(&infoStr, " PALETTE: %d\n", ui.palette)
	fmt.Fprintf(&infoStr, " STATUS: %s\n", info.StatusString())
	fmt.Fprintf(&infoStr, " PC: %04X\n", info.PC)
	fmt.Fprintf(&infoStr, " A: $%02X [%03d]", info.A, info.A)
	fmt.Fprintf(&infoStr, " X: $%02X [%03d]", info.X, info.X)
	fmt.Fprintf(&infoStr, " Y: $%02X [%03d]\n", info.Y, info.Y)
	fmt.Fprintf(&infoStr, " SP: $%02X\n", info.SP)

	for i := max(0, info.PC-7); i < info.PC; i++ {
		infoStr.WriteString(" " + ui.disasm[i] + "\n")
	}
	infoStr.WriteString("*" + ui.disasm[info.PC] + "\n")
	for i := info.PC + 1; i < min(0xFFFF, info.PC+7); i++ {
		infoStr.WriteString(" " + ui.disasm[i] + "\n")
	}

	debugScreenOffsetX := float32(gameScreenWidth * gameScreenScale)
	vector.DrawFilledRect(screen, debugScreenOffsetX, 0, debugScreenWidth, debugScreenHeight, color.RGBA{50, 50, 50, 255}, false)
	ebitenutil.DebugPrintAt(screen, infoStr.String(), int(debugScreenOffsetX), 0)

	for i := 0; i < 8; i++ {
		paletteImg := ebiten.NewImage(4, 1)
		paletteImg.Set(0, 0, ui.bus.GetColorFromPalette(uint8(i), 0))
		paletteImg.Set(1, 0, ui.bus.GetColorFromPalette(uint8(i), 1))
		paletteImg.Set(2, 0, ui.bus.GetColorFromPalette(uint8(i), 2))
		paletteImg.Set(3, 0, ui.bus.GetColorFromPalette(uint8(i), 3))

		op := &ebiten.DrawImageOptions{}
		op.GeoM.Scale(4, 4)
		op.GeoM.Translate(float64(debugScreenOffsetX)+10+float64(i*35), debugScreenHeight-128-20)
		screen.DrawImage(paletteImg, op)

	}

	for i := 0; i < 2; i++ {
		tilesImg := ebiten.NewImageFromImage(ui.bus.GetPatternTable(ui.palette, uint8(i)))
		op := &ebiten.DrawImageOptions{}
		op.GeoM.Translate(float64(debugScreenOffsetX)+10+(float64(i)*(128+5)), debugScreenHeight-128-10)
		screen.DrawImage(tilesImg, op)
	}

	img := ebiten.NewImageFromImage(ui.bus.Screen())
	op := &ebiten.DrawImageOptions{}
	op.GeoM.Scale(gameScreenScale, gameScreenScale)
	screen.DrawImage(img, op)
}

const (
	gameScreenScale  = 2
	gameScreenWidth  = 256
	gameScreenHeight = 240

	debugScreenWidth  = 286
	debugScreenHeight = gameScreenHeight * gameScreenScale
)

func (ui *UI) Layout(_, _ int) (int, int) {
	return gameScreenWidth*gameScreenScale + debugScreenWidth, gameScreenHeight * gameScreenScale
}

func RunUI(ui *UI) error {
	ebiten.SetWindowResizingMode(ebiten.WindowResizingModeEnabled)
	screenSizeX, screenSizeY := gameScreenWidth*gameScreenScale+debugScreenWidth, gameScreenHeight*gameScreenScale
	screenSizeX *= 2
	screenSizeY *= 2
	ebiten.SetWindowSize(screenSizeX, screenSizeY)
	ebiten.SetTPS(60)
	return ebiten.RunGame(ui)
}
