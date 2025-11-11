package common.util;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.lang.StringUtils;

import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.VersionUtils;
import eim.util.WorkFlowUtils;
import eimtest.app.tool.CreateMissingFile;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class AppLogicUtil_TreeWalkerTest extends TestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(AppLogicUtil_TreeWalkerTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					EIMObject obj = TestAppObjectUtil.getObj(sess, "F1");
					ObjectUtils.rename(sess, obj, "WS1F1");
					new CreateFolderTreeData(sess, TestAppMisc.getAppFilePath(
						AppLogicUtil_TreeWalkerTest.class, ".xls")).process();
					sess.commit();
					new CreateMissingFile().start(null);
				}
			}
		};
		return w;
	}

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	/** */
	List objs;

	/** */
	public void setUp() throws Exception {
		sess = TestSessionUtil.createEIMSession();
		objs = TestAppObjectUtil.getFolderAndDocObjs(sess);
	}

	protected void tearDown() throws Exception {
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testProcessFolderTree() throws Exception {
		// d1.txtの履歴を2つ作る
		EIMObject d1 = TestAppObjectUtil.findObj("d1.txt", objs);
		// 履歴1つめ
		EIMObject d1up = VersionUtils.revisionUp(sess, d1);
		RelationUtils.deleteRelation(sess, (EIMRelation) RelationUtils.getParentRelationList(sess,
			d1).get(0));
		ObjectUtils.rename(sess, d1up, "d1r1.txt");
		// 公開中まで持っていく(一度差し戻し有り)
		WorkFlowUtils.statusUp(sess, d1up);
		WorkFlowUtils.statusDown(sess, d1up);
		WorkFlowUtils.statusUp(sess, d1up);
		WorkFlowUtils.statusUp(sess, d1up);
		WorkFlowUtils.statusUp(sess, d1up);
		d1 = d1up;
		// 履歴2つめ
		d1up = VersionUtils.revisionUp(sess, d1);
		ObjectUtils.rename(sess, d1up, "d1r2.txt");

		MyProcessFolderTreeWalker walker = new MyProcessFolderTreeWalker();
		AppLogicUtil.processFolderTree(sess, TestAppObjectUtil.findObj("F1", objs), false, walker,
			null);
		assertEquals(TestAppMisc.loadAppTextFile(this, "_01.txt"), walker.toString());

		walker = new MyProcessFolderTreeWalker();
		AppLogicUtil.processFolderTree(sess, TestAppObjectUtil.findObj("F1", objs), true, walker,
			null);
		assertEquals(TestAppMisc.loadAppTextFile(this, "_02.txt"), walker.toString());
	}

	/** */
	class MyProcessFolderTreeWalker implements AppLogicUtil.ProcessFolderTreeWalker {
		/** */
		List calledHist = new ArrayList();

		/** */
		int level = 0;

		public void processDocument(EIMObject object, AppObjectConditionHelper helper)
				throws Exception {
			add(" @:" + object.getName() + ":" + object.getRevision());
		}

		public void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
				AppObjectConditionHelper helper) throws Exception {
			add(">>:" + ((upperFolderObjFrom == null) ? "-" : upperFolderObjFrom.getName()) + "→"
					+ lowerFolderObjTo.getName());
			level++;
		}

		public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
				AppObjectConditionHelper helper) throws Exception {
			add("<<:" + ((upperFolderObjTo == null) ? "-" : upperFolderObjTo.getName()) + "←"
					+ lowerFolderObjFrom.getName());
			level--;
		}

		/**
		 * 
		 * @param str
		 */
		void add(String str) {
			calledHist.add(StringUtils.repeat("  ", level) + str);
		}

		public String toString() {
			StringWriter sw = new StringWriter();
			PrintWriter pw = new PrintWriter(sw);
			for (Iterator i = calledHist.iterator(); i.hasNext();) {
				pw.println(i.next());
			}
			pw.close();
			return sw.toString();
		}
	};
}
