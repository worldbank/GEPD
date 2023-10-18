import xml.etree.ElementTree as ET

#read xml file at C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\version_1\Questionnaire\ddi_short.xml
xml_str= open(r'C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\version_1\Questionnaire\ddi_short.xml', 'r').read()

root = ET.fromstring(xml_str)

table_str = '| Name | File |  Type | Label |\n|  --- | --- | --- | --- |\n'
for var in root.findall('.//var'):
    var_id = var.get('ID')
    name = var.get('name')
    files = var.get('files')
    intrvl = var.get('intrvl')
    label = var.find('labl').text
    table_str += f'|  {name} | {files} | {intrvl} | {label} |\n'

#save results as markdown file
with open(r'C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\version_1\Questionnaire\ddi_short.md', 'w') as f:
    f.write(table_str)

# Data
xml_str = open(r'C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\version_1\Questionnaire\ddi_data_short.xml', 'r').read()


root = ET.fromstring(xml_str)

table_str = '| ID | Name |\n| --- | --- |\n'
for fileDscr in root.findall('.//fileDscr'):
    file_id = fileDscr.get('ID')
    file_name = fileDscr.find('.//fileName').text
    table_str += f'| {file_id} | {file_name} |\n'

print(table_str)

#save results as markdown file
with open(r'C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\version_1\Questionnaire\ddi_data_short.md', 'w') as f:
    f.write(table_str)
    
    
# Survey of Public Officials


#read xml file at C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\version_1\Questionnaire\ddi_short.xml
xml_str= open(r'C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\Public_Officials\Questionnaire\ddi_short.xml', 'r').read()

root = ET.fromstring(xml_str)

table_str = '| Name | File |  Type | Label |\n|  --- | --- | --- | --- |\n'
for var in root.findall('.//var'):
    var_id = var.get('ID')
    name = var.get('name')
    files = var.get('files')
    intrvl = var.get('intrvl')
    label = var.find('labl').text
    table_str += f'|  {name} | {files} | {intrvl} | {label} |\n'

#save results as markdown file
with open(r'C:\Users\wb469649\WBG\HEDGE Files - HEDGE Documents\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\Public_Officials\Questionnaire\ddi_short.md', 'w') as f:
    f.write(table_str)

