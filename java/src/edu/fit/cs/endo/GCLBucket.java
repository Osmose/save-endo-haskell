package edu.fit.cs.endo;

public class GCLBucket {
	private int red;
	private int green;
	private int blue;
	private int colorCount;
	
	private int alpha;
	private int alphaCount;
	
	public int getColor() {
		int a = 255;
		if (alphaCount > 0) {
			a = alpha / alphaCount;
		}
		
		int r = 0, g = 0, b = 0;
		if (colorCount > 0) {
			double mult = ((double)a) / 255.0f;
			r = (int) ((red / colorCount) * mult);
			g = (int) ((green / colorCount) * mult);
			b = (int) ((blue / colorCount) * mult);
		}
		
		return GCLBitmap.componentsToARGB(r, g, b, a);
	}
	
	public void addColor(GCLColor color) {
		red += color.getRed();
		green += color.getGreen();
		blue += color.getBlue();
		colorCount++;
	}
	
	public void addAlpha(GCLAlpha a) {
		alpha += a.getAlpha();
		alphaCount++;
	}
	
	public void empty() {
		red = 0;
		green = 0;
		blue = 0;
		alpha = 0;
		colorCount = 0;
		alphaCount = 0;
	}
}
