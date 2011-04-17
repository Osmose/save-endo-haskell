package edu.fit.cs.endo;
import java.util.HashMap;

public enum GCLCommand {
	BLACK,
	RED,
	GREEN,
	YELLOW,
	BLUE,
	MAGENTA,
	CYAN,
	WHITE,
	TRANS,
	OPAQUE,
	EMPTY,
	MOVE,
	CCW,
	CW,
	MARK,
	LINE,
	FILL,
	ADD,
	COMPOSE,
	CLIP,
	UNKNOWN;
	
	private static HashMap<String, GCLCommand> strCmdMap;
	
	private static void initStrCmdMap() {
		strCmdMap = new HashMap<String, GCLCommand>();
		strCmdMap.put("PIPIIIC", BLACK);
		strCmdMap.put("PIPIIIP", RED);
		strCmdMap.put("PIPIICC", GREEN);
		strCmdMap.put("PIPIICF", YELLOW);
		strCmdMap.put("PIPIICP", BLUE);
		strCmdMap.put("PIPIIFC", MAGENTA);
		strCmdMap.put("PIPIIFF", CYAN);
		strCmdMap.put("PIPIIPC", WHITE);
		strCmdMap.put("PIPIIPF", TRANS);
		strCmdMap.put("PIPIIPP", OPAQUE);
		strCmdMap.put("PIIPICP", EMPTY);
		strCmdMap.put("PIIIIIP", MOVE);
		strCmdMap.put("PCCCCCP", CCW);
		strCmdMap.put("PFFFFFP", CW);
		strCmdMap.put("PCCIFFP", MARK);
		strCmdMap.put("PFFICCP", LINE);
		strCmdMap.put("PIIPIIP", FILL);
		strCmdMap.put("PCCPFFP", ADD);
		strCmdMap.put("PFFPCCP", COMPOSE);
		strCmdMap.put("PFFICCF", CLIP);
	}
	
	public static GCLCommand strToCommand(String str) {
		if (strCmdMap == null) {
			initStrCmdMap();
		}
		
		if (strCmdMap.get(str) == null) {
			return UNKNOWN;
		}
		
		return strCmdMap.get(str);
	}
}
