package edu.fit.cs.endo;
public enum GCLColor {
	BLACK(0, 0, 0),
	RED(255, 0, 0),
	GREEN(0, 255, 0),
	YELLOW(255, 255, 0),
	BLUE(0, 0, 255),
	MAGENTA(255, 0, 255),
	CYAN(0, 255, 255),
	WHITE(255, 255, 255);
	
	private final int red;
	private final int green;
	private final int blue;
	
	private GCLColor(int r, int g, int b) {
		red = r;
		green = g;
		blue = b;
	}

	public int getRed() {
		return red;
	}

	public int getGreen() {
		return green;
	}

	public int getBlue() {
		return blue;
	}
}
