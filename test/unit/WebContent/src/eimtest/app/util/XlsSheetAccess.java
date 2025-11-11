package eimtest.app.util;

import java.io.FileInputStream;
import java.util.Stack;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;

/** */
public class XlsSheetAccess {
	/** */
	HSSFWorkbook wb;

	/** */
	HSSFSheet sheet;

	/** */
	public int lastRow;

	/** */
	public int lastCol;

	/** */
	private Stack posMark = new Stack();

	/**
	 * 
	 * @param fileName
	 * @throws Exception
	 */
	public XlsSheetAccess(String fileName) throws Exception {
		FileInputStream fis = new FileInputStream(fileName);
		POIFSFileSystem fs = new POIFSFileSystem(fis);
		fis.close();
		wb = new HSSFWorkbook(fs);
		sheet = wb.getSheetAt(0);
	}

	/**
	 * 
	 * @param row
	 * @param col
	 * @return o
	 */
	public String get(int row, int col) {
		seek(row, col);
		return get();
	}

	/**
	 * 
	 * @return o
	 */
	public String get() {
		if (lastRow >= 65536 || lastCol >= 256)
			throw new IllegalStateException("out of sheet range.row=" + lastRow + ",col=" + lastCol);
		HSSFRow row = sheet.getRow((short) (lastRow - 1));
		if (row == null)
			return "";
		HSSFCell cell = row.getCell((short) (lastCol - 1));
		if (cell == null)
			return "";
		switch (cell.getCellType()) {
		case HSSFCell.CELL_TYPE_NUMERIC:
			return String.valueOf(cell.getNumericCellValue());
		case HSSFCell.CELL_TYPE_BOOLEAN:
			return String.valueOf(cell.getBooleanCellValue());
		case HSSFCell.CELL_TYPE_BLANK:
			return "";
		default:
			return cell.getRichStringCellValue().getString();
		}
	}

	/**
	 * 
	 * @param row
	 * @param col
	 * @return o
	 */
	public XlsSheetAccess seek(int row, int col) {
		if (row > 0)
			lastRow = row;
		if (col > 0)
			lastCol = col;
		return this;
	}

	/**
	 * 
	 * @return o
	 */
	public XlsSheetAccess nextRow() {
		lastRow++;
		return this;
	}

	/**
	 * 
	 * @return o
	 */
	public XlsSheetAccess prevRow() {
		lastRow--;
		return this;
	}

	/**
	 * 
	 * @return o
	 */
	public XlsSheetAccess nextCol() {
		lastCol++;
		return this;
	}

	/**
	 * 
	 * @return o
	 */
	public XlsSheetAccess prevCol() {
		lastCol--;
		return this;
	}

	/**	 */
	public void pushMark() {
		posMark.push(new int[] { lastCol, lastRow });
	}

	/**	 */
	public void popMark() {
		int[] pos = (int[]) posMark.pop();
		lastCol = pos[0];
		lastRow = pos[1];
	}
}