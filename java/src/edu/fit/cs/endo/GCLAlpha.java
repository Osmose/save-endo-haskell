package edu.fit.cs.endo;
public enum GCLAlpha {
	TRANSPARENT(0),
	OPAQUE(255);
	
	private final int alpha;
	
	private GCLAlpha(int a) {
		alpha = a;
	}
	
	public int getAlpha() {
		return alpha;
	}
}
