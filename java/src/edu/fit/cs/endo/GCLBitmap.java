package edu.fit.cs.endo;
import java.awt.image.BufferedImage;


public class GCLBitmap {
	public static final int COMBINE_COMPOSE = 0;
	public static final int COMBINE_CLIP = 1;
	
	public static final int COLOR_ALPHA = 0;
	public static final int COLOR_RED = 1;
	public static final int COLOR_GREEN = 2;
	public static final int COLOR_BLUE = 3;
	
	private int[] data;
	private int width;
	private int height;
	
	public GCLBitmap() {
		this(600, 600);
	}
	
	public GCLBitmap(int w, int h) {
		width = w;
		height = h;
		data = new int[w * h];
	}
	
	public int get(int x, int y) {
		return data[x + (y * width)];
	}
	
	public void set(int x, int y, int val) {
		data[x + (y * width)] = val;
	}
	
	public static void combine(GCLBitmap bmp0, GCLBitmap bmp1, int action) {
		int[] colors0 = new int[4];
		int[] colors1 = new int[4];
		int color0, color1;
		for (int k = 0; k < (600 * 600); k++) {
			color0 = bmp0.data[k];
			ARGBToComponents(colors0, color0);
		
			color1 = bmp1.data[k];
			ARGBToComponents(colors1, color1);
			
			switch (action) {
				case COMBINE_COMPOSE:
					bmp1.data[k] = compose.combine(k, colors0, colors1);
					break;
				case COMBINE_CLIP:
					bmp1.data[k] = clip.combine(k, colors0, colors1);
					break;
			}
		}
	}
	
	private static GCLCombine compose = new GCLCombine() {
		public int combine(int index, int[] colors0, int[] colors1) {
			double mult = (255 - colors0[COLOR_ALPHA]) / 255.0f;
			return componentsToARGB(
				colors0[COLOR_RED] + (int)(colors1[COLOR_RED] * mult),
				colors0[COLOR_GREEN] + (int)(colors1[COLOR_GREEN] * mult),
				colors0[COLOR_BLUE] + (int)(colors1[COLOR_BLUE] * mult),
				colors0[COLOR_ALPHA] + (int)(colors1[COLOR_ALPHA] * mult)
			);
		}
	};
	
	private static GCLCombine clip = new GCLCombine() {
		public int combine(int index, int[] colors0, int[] colors1) {
			double mult = colors0[COLOR_ALPHA] / 255.0f;
			return componentsToARGB(
				(int)(colors1[COLOR_RED] * mult),
				(int)(colors1[COLOR_GREEN] * mult),
				(int)(colors1[COLOR_BLUE] * mult),
				(int)(colors1[COLOR_ALPHA] * mult)
			);
		}
	};
	
	public BufferedImage toBufferedImage() {
		BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		img.setRGB(0, 0, width, height, data, 0, width);
		
		return img;
	}
	
	public static int componentsToARGB(int r, int g, int b, int a) {
		return ((a & 0xFF) << 24) + ((r & 0xFF) << 16) + ((g & 0xFF) << 8) + (b & 0xFF);
	}
	
	public static void ARGBToComponents(int[] arr, int color) {
		arr[COLOR_ALPHA] = (color & 0xFF000000) >>> 24;
		arr[COLOR_RED]	 = (color & 0x00FF0000) >>> 16;
		arr[COLOR_GREEN] = (color & 0x0000FF00) >>> 8;
		arr[COLOR_BLUE]  = (color & 0x000000FF);
	}
}
