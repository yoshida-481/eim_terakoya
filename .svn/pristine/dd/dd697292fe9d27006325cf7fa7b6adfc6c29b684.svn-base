package eimtest.app.tool;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.ResultSet;

import eim.bo.EIMDirectory;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eimtest.util.TestDBUtil;
import eimtest.util.TestSessionUtil;

/** */
public class CreateMissingFile {
	/** */
	EIMSession sess = TestSessionUtil.createEIMSession();

	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		new CreateMissingFile().start(args);
	}

	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public void start(String[] args) throws Exception {
		System.out.println("start " + this.getClass());
		System.out.println(TestDBUtil.getConData(sess.getDBConnection()));
		java.sql.Statement stmt = sess.getDBConnection().createStatement();
		ResultSet rs = stmt.executeQuery("select * from eimfile");
		while (rs.next()) {
			int objId = rs.getInt("id");
			int dirId = rs.getInt("dir");
			String name = rs.getString("name");
			String ext = rs.getString("ext");
			EIMDirectory dir = FileUtils.getDirectoryById(sess, dirId);
			EIMObject obj = ObjectUtils.getObjectById(sess, objId);

			if (obj == null) {
				System.out.println("invalid data skipped. obj not found." + objId + "," + name);
				continue;
			}

			if (obj.getAttribute("パス") == null) {
				System.out.println("invalid data skipped. obj=" + obj.getId() + "," + obj.getName());
				continue;
			}

			File f = new File(dir.getPath() + objId + ext);
			if (!f.exists()) {
				f.getParentFile().mkdirs();
				PrintWriter pw = new PrintWriter(new OutputStreamWriter(new FileOutputStream(f),
						"utf-8"));
				pw.print(objId + "," + obj.getAttribute("パス").getString() + name);
				pw.close();
				System.out.println(" created " + f.getAbsolutePath());
			}
		}
		System.out.println("fin");
	}
}
