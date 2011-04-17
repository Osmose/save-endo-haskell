package edu.fit.cs.endo;
import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;
import java.util.Timer;
import java.util.TimerTask;

import javax.imageio.ImageIO;
import javax.swing.JApplet;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

public class SaveEndo extends JApplet implements ActionListener {
	// Endo State
	private Point pos;
	private GCLDirection dir;
	private Point mark;
	private GCLBucket bucket = new GCLBucket();
	private Stack<GCLBitmap> bitmaps = new Stack<GCLBitmap>();
	
	// Execution state
	LinkedList<GCLCommand> cmds, cmdsBackup;
	
	// GUI Stuff
	private FileDialog fd;
	private EndoScreen screen;
	private Timer executeTimer = new Timer();
	private TimerTask executeTimerTask = new TimerTask() {
		public void run() {
			if (!cmds.isEmpty()) {
				executeToChange();
			}
		}
	};
	
	public SaveEndo() {
		fd = new FileDialog(new Frame(), "Choose an RNA file", FileDialog.LOAD);
		initMenu();
		initGUI();
		
		setVisible(true);
	}
	
	private void initMenu() {
		JMenuBar menuBar = new JMenuBar();
		setJMenuBar(menuBar);
		
		JMenu fileMenu = new JMenu("File");
		fileMenu.setMnemonic(KeyEvent.VK_F);
		menuBar.add(fileMenu);
		
		JMenuItem selftest = new JMenuItem("Self-Test");
		selftest.addActionListener(this);
		fileMenu.add(selftest);
		
		JMenuItem runendo = new JMenuItem("Run Endo");
		runendo.addActionListener(this);
		fileMenu.add(runendo);
	}
	
	private void initGUI() {
		screen = new EndoScreen();
		add(screen, BorderLayout.CENTER);
	
		JPanel buttons = new JPanel();
		
		JButton playButton = new JButton("Play");
		playButton.setActionCommand("play");
		playButton.addActionListener(this);
		buttons.add(playButton);
		
		JButton playFastButton = new JButton("Play Fast");
		playFastButton.setActionCommand("playfast");
		playFastButton.addActionListener(this);
		buttons.add(playFastButton);
		
		JButton stopButton = new JButton("Stop");
		stopButton.setActionCommand("stop");
		stopButton.addActionListener(this);
		buttons.add(stopButton);
		
		JButton nextButton = new JButton("Next");
		nextButton.setActionCommand("next");
		nextButton.addActionListener(this);
		buttons.add(nextButton);
		
		JButton finishButton = new JButton("Finish");
		finishButton.setActionCommand("finish");
		finishButton.addActionListener(this);
		buttons.add(finishButton);
		
		JButton resetButton = new JButton("Reset");
		resetButton.setActionCommand("reset");
		resetButton.addActionListener(this);
		buttons.add(resetButton);
		
		JButton toggleBGButton = new JButton("Toggle BG");
		toggleBGButton.setActionCommand("togglebg");
		toggleBGButton.addActionListener(this);
		buttons.add(toggleBGButton);
		
		add(buttons, BorderLayout.SOUTH);
	}
	
	private void openFile(String file) {
		try {
			InputStream is = getClass().getResourceAsStream(file);
			byte[] data = new byte[7];
			cmds = new LinkedList<GCLCommand>();
			
			while (is.read(data) != -1) {
				cmds.add(GCLCommand.strToCommand(new String(data)));
			}
			is.close();
			
			cmdsBackup = new LinkedList<GCLCommand>();
			cmdsBackup.addAll(cmds);
			
			reset();
			screen.setImg(bitmaps.peek().toBufferedImage());
		} catch (Exception err) {
			System.out.println("Error reading file: " + file);
			System.out.println(err.toString());
			err.printStackTrace();
		}
	}
	
	private void reset() {
		pos = new Point(0, 0);
		dir = GCLDirection.EAST;
		mark = new Point(0, 0);
		bucket.empty();
		bitmaps.clear();
		bitmaps.push(new GCLBitmap());
	}
	
	public boolean executeGCL(GCLCommand cmd) {
		switch (cmd) {
			// Movement
			case MOVE:	move(); return false;
			case MARK:	mark(); return false;
			case CW:	cw(); return false;
			case CCW:	ccw(); return false;
			
			// Bucket
			case EMPTY: empty(); return false;
			
			// Colors
			case BLACK:		addColor(GCLColor.BLACK); return false;
			case RED:		addColor(GCLColor.RED); return false;
			case GREEN:		addColor(GCLColor.GREEN); return false;
			case YELLOW: 	addColor(GCLColor.YELLOW); return false;
			case BLUE:		addColor(GCLColor.BLUE); return false;
			case MAGENTA: 	addColor(GCLColor.MAGENTA); return false;
			case CYAN:		addColor(GCLColor.CYAN); return false;
			case WHITE:		addColor(GCLColor.WHITE); return false;
			
			// Alpha
			case TRANS:		addAlpha(GCLAlpha.TRANSPARENT); return false;
			case OPAQUE:	addAlpha(GCLAlpha.OPAQUE); return false;
			
			// Bitmap commands
			case LINE: line(); return true;
			case FILL: fill(); return true;
			case ADD: add(); return true;
			case COMPOSE: combine(GCLBitmap.COMBINE_COMPOSE); return true;
			case CLIP: combine(GCLBitmap.COMBINE_CLIP); return true;
			
			default: return false;
		}
	}
	
	private void move() {
		switch (dir) {
			case NORTH: pos.y = (pos.y + 599) % 600; break;
			case SOUTH: pos.y = (pos.y +   1) % 600; break;
			case WEST: 	pos.x = (pos.x + 599) % 600; break;
			case EAST: 	pos.x = (pos.x +   1) % 600; break;
		}
	}
	
	private void mark() {
		mark.setLocation(pos);
	}
	
	private void cw() {
		switch (dir) {
			case NORTH: dir = GCLDirection.EAST; break;
			case EAST: dir = GCLDirection.SOUTH; break;
			case SOUTH: dir = GCLDirection.WEST; break;
			case WEST: dir = GCLDirection.NORTH; break;
		}
	}
	
	private void ccw() {
		switch (dir) {
			case NORTH: dir = GCLDirection.WEST; break;
			case WEST: dir = GCLDirection.SOUTH; break;
			case SOUTH: dir = GCLDirection.EAST; break;
			case EAST: dir = GCLDirection.NORTH; break;
		}
	}
	
	private void empty() {
		bucket.empty();
	}
	
	private void addColor(GCLColor color) {
		bucket.addColor(color);
	}
	
	private void addAlpha(GCLAlpha alpha) {
		bucket.addAlpha(alpha);
	}
	
	private void line() {
		int dx = mark.x - pos.x;
		int dy = mark.y - pos.y;
		int d = Math.max(Math.abs(dx), Math.abs(dy));
		
		int c = (dx * dy <= 0 ? 1 : 0);
		int z = ((d - c) / 2);
		int x = (pos.x * d) + z;
		int y = (pos.y * d) + z;
		
		GCLBitmap bitmap = bitmaps.peek();
		for (int k = 0; k < d; k++) {
			bitmap.set(x / d, y / d, bucket.getColor());
			x += dx;
			y += dy;
		}
	}
	
	private void fill() {
		GCLBitmap bmp = bitmaps.peek();
		int search = bmp.get(pos.x, pos.y);
		int fill = bucket.getColor();
		
		if (search != fill) {
			Queue<Point> q = new LinkedList<Point>();
			q.add(pos);
			
			Point p;
			while (!q.isEmpty()) {
				p = q.poll();
				if (bmp.get(p.x, p.y) == search) {
					bmp.set(p.x, p.y, fill);
					
					if (p.x > 0)	q.add(new Point(p.x - 1, p.y));
					if (p.x < 599)	q.add(new Point(p.x + 1, p.y));
					if (p.y > 0)	q.add(new Point(p.x, p.y - 1));
					if (p.y < 599)	q.add(new Point(p.x, p.y + 1));
				}
			}
		}
	}
	
	private void add() {
		if (bitmaps.size() < 10) {
			bitmaps.push(new GCLBitmap());
		}
	}
	
	private void combine(int action) {
		if (bitmaps.size() > 1) {
			GCLBitmap bmp0 = bitmaps.pop();
			GCLBitmap bmp1 = bitmaps.peek();
			
			GCLBitmap.combine(bmp0, bmp1, action);
		}
	}
	
	public void outputImage(String filename) throws IOException {
		BufferedImage img = bitmaps.peek().toBufferedImage();
		
		File f = new File(filename);
		ImageIO.write(img, "png", f);
	}
	
	private void executeToChange() {
		if (!cmds.isEmpty()) {
			while(!cmds.isEmpty() && !executeGCL(cmds.pop())) {};
			screen.setImg(bitmaps.peek().toBufferedImage());
		}
	}
	
	private void executeToEnd() {
		if (!cmds.isEmpty()) {
			while (!cmds.isEmpty()) {
				executeGCL(cmds.pop());
			}
			screen.setImg(bitmaps.peek().toBufferedImage());
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Self-Test")) {
			openFile("/data/self.rna");
		} else if (e.getActionCommand().equals("Run Endo")) {
			openFile("/data/null.rna");
		} else if (e.getActionCommand().equals("next")) {
			executeToChange();
		} else if (e.getActionCommand().equals("finish")) {
			executeToEnd();
		} else if (e.getActionCommand().equals("reset")) {
			reset();
			cmds.clear();
			cmds.addAll(cmdsBackup);
			screen.setImg(bitmaps.peek().toBufferedImage());
		} else if (e.getActionCommand().equals("play")) {
			executeTimer.schedule(executeTimerTask, 0, 100);
		} else if (e.getActionCommand().equals("playfast")) {
			executeTimer.schedule(executeTimerTask, 0, 10);
		} else if (e.getActionCommand().equals("stop")) {
			executeTimer.cancel();
		} else if (e.getActionCommand().equals("togglebg")) {
			screen.toggleBGColor();
		}
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("- ENDO -\n");
		sb.append("Position:  " + pos.toString() + "\n");
		sb.append("Mark:      " + mark.toString() + "\n");
		sb.append("Direction: " + dir.toString() + "\n");
		
		int[] c = new int[4];
		GCLBitmap.ARGBToComponents(c, bucket.getColor());
		sb.append("Color:     (("+c[1]+","+c[2]+","+c[3]+"),"+c[0]+")");
		
		return sb.toString();
	}

	public static void main(String[] args) {
		SaveEndo endo = new SaveEndo();
	}
}
