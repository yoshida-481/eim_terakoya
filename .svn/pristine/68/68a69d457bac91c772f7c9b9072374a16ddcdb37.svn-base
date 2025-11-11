package app.document.session;

import java.io.File;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;
import org.apache.xpath.XPathAPI;

import app.document.DocumentService;
import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAccessRole;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.GroupUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DspSessionTest extends TestCase {
	/** */
	DocumentService ds;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	/** */
	EIMUser u1;

	/** */
	EIMUser u2;

	/** */
	EIMUser u3;

	/** */
	EIMUser u4;

	public void setUp() throws Exception
	{
		TestAppDBUtil.loadPrimitiveData();
		ds = new DocumentService("u1", "u1");
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		u1 = UserUtils.getUserByCode(sess, "u1");
		u2 = UserUtils.getUserByCode(sess, "u2");
		u3 = UserUtils.getUserByCode(sess, "u3");
		u4 = UserUtils.getUserByCode(sess, "u4");
		setupSecurity();
	}

	/**
	 * 
	 * @throws Exception
	 */
	void setupSecurity() throws Exception
	{
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, "sec");
		EIMAccessEntry entry = SecurityUtils.createAccessEntry(sess, sec, new EIMAccessEntryType(
				EIMAccessEntryType.USER), u2);
		SecurityUtils.updatePriority(sess, sec, entry, 1);
		SecurityUtils.updateAccessRole(sess, entry, new EIMAccessRole(EIMAccessRole.READ), 1);
		sess.commit();
	}

	protected void tearDown() throws Exception
	{
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception
	{
		// login is 'u1'
		// create document for request wf
		File file = new File(this.getClass().getResource(
				ClassUtils.getShortClassName(this.getClass()) + ".class").getPath());
		EIMObjectType docType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		// create document
		ds.object_actCreateDocument("F1", docType, "pp"//
				, u1.getId()//
				, "", file);
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd())//
				.addCondition(
						h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, "パス"),
								"/WS1/F1/"))//
				.addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.NAME, file.getName())//
				));
		EIMObject docObj = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);

		// request approve
		EIMGroup g = (EIMGroup) GroupUtils.getGroupByUser(sess, u2).get(0);
		ds.approve_actRequestApprove(docObj.getId(), "2:" + g.getId(), new String[] {},
				"approve as fast as!", true, false);

		// @Test: 1 approve requested to u3
		switchServiceUser("u3");
		assertEquals("1", XPathAPI.selectSingleNode(xu.toDOM(ds.session_dspSession()),
				"user/@approveDocument").getNodeValue());

		// @Test: no approve requested to u2
		switchServiceUser("u2");
		assertEquals("0", XPathAPI.selectSingleNode(xu.toDOM(ds.session_dspSession()),
				"user/@approveDocument").getNodeValue());
	}

	/**
	 * 
	 * @param userCode
	 * @throws Exception
	 */
	void switchServiceUser(String userCode) throws Exception
	{
		ds.br.eimLogin(userCode, userCode, null, null);
	}
}
