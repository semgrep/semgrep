# This file is part of Scapy
# See http://www.secdev.org/projects/scapy for more information
# Copyright (C) Nils Weiss <nils@we155.de>
# Copyright (C) Jonas Schmidt <jonas.schmidt@st.othr.de>
# This program is published under a GPLv2 license
# scapy.contrib.description = Volkswagen specific definitions for UDS
# scapy.contrib.status = skip
from scapy.contrib.automotive.uds import UDS_RC
from scapy.contrib.automotive.uds import UDS_RD
from scapy.contrib.automotive.uds import UDS_RDBI


UDS_RDBI.dataIdentifiers[0x00BD] = "Theft Protection - Download GFA-Key"
UDS_RDBI.dataIdentifiers[0x00BE] = "Theft Protection - Download IKA-Key"
UDS_RDBI.dataIdentifiers[0x00FD] = "IUMPR-ID3"
UDS_RDBI.dataIdentifiers[0x00FE] = "IUMPR-ID2"
UDS_RDBI.dataIdentifiers[0x00FF] = "IUMPR-ID1"
UDS_RDBI.dataIdentifiers[0x02CC] = "Vehicle_identification_number_provisional"
UDS_RDBI.dataIdentifiers[0x02E0] = "Immobilizer - Challenge"
UDS_RDBI.dataIdentifiers[0x02E1] = "Immobilizer - Login"
UDS_RDBI.dataIdentifiers[0x02E2] = "Immobilizer - Download Powertrain"
UDS_RDBI.dataIdentifiers[0x02E3] = "Immobilizer - Download IMS"
UDS_RDBI.dataIdentifiers[0x02E4] = "Transponder ID current Key"
UDS_RDBI.dataIdentifiers[0x02E5] = "Transponder ID Key 1"
UDS_RDBI.dataIdentifiers[0x02E6] = "Transponder ID Key 2"
UDS_RDBI.dataIdentifiers[0x02E7] = "Transponder ID Key 3"
UDS_RDBI.dataIdentifiers[0x02E8] = "Transponder ID Key 4"
UDS_RDBI.dataIdentifiers[0x02E9] = "Transponder ID Key 5"
UDS_RDBI.dataIdentifiers[0x02EA] = "Transponder ID Key 6"
UDS_RDBI.dataIdentifiers[0x02EB] = "Transponder ID Key 7"
UDS_RDBI.dataIdentifiers[0x02EC] = "Transponder ID Key 8"
UDS_RDBI.dataIdentifiers[0x02ED] = "State of Immobilizer"
UDS_RDBI.dataIdentifiers[0x02EE] = "State of Immobilizer Slaves"
UDS_RDBI.dataIdentifiers[0x02EF] = "State Blocking Time"
UDS_RDBI.dataIdentifiers[0x02F1] = "Immobilizer - Slave Login"
UDS_RDBI.dataIdentifiers[0x02F6] = "Download WFS SHE"
UDS_RDBI.dataIdentifiers[0x02F9] = "CRC32 Checksum of FAZIT Identification String"
UDS_RDBI.dataIdentifiers[0x02FA] = "Adapted_transponders_checksum"
UDS_RDBI.dataIdentifiers[0x02FB] = "Immobilizer - Download WFS 4"
UDS_RDBI.dataIdentifiers[0x02FF] = "Immobilizer_snapshot"
UDS_RDBI.dataIdentifiers[
    0x0407
] = "VW Logical Software Block Counter Of Programming Attempts"
UDS_RDBI.dataIdentifiers[0x040F] = "VW Logical Software Block Lock Value"
UDS_RDBI.dataIdentifiers[0x0410] = "Bootloader TP Blocksize"
UDS_RDBI.dataIdentifiers[0x04A3] = "Gateway Component List"
UDS_RDBI.dataIdentifiers[0x0600] = "VW Coding Value"
UDS_RDBI.dataIdentifiers[0x0610] = "Control_unit_for_wiper_motor_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0611] = "Slave_list_VW_spare_part_number"
UDS_RDBI.dataIdentifiers[0x0612] = "Slave_list_VW_software_version_number"
UDS_RDBI.dataIdentifiers[0x0613] = "Slave_list_VW_ecu_hardware_version_number"
UDS_RDBI.dataIdentifiers[0x0614] = "Slave_list_VW_hardware_number"
UDS_RDBI.dataIdentifiers[0x0615] = "Slave_list_ecu_serial_number"
UDS_RDBI.dataIdentifiers[0x0616] = "Slave_list_VW_FAZIT_identification_string"
UDS_RDBI.dataIdentifiers[0x0617] = "Slave_list_VW_system_name_or_engine_type"
UDS_RDBI.dataIdentifiers[
    0x0618
] = "Left_rear_seat_ventilation_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x0619
] = "Right_rear_seat_ventilation_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x061A] = "Slave_component_list"
UDS_RDBI.dataIdentifiers[0x061B] = "Slave_component_list_databus_identification"
UDS_RDBI.dataIdentifiers[0x061C] = "Slave_component_list_ecu_identification"
UDS_RDBI.dataIdentifiers[0x061D] = "Slave_component_list_present"
UDS_RDBI.dataIdentifiers[0x061E] = "Right_headlamp_power_output_stage_Coding_Values"
UDS_RDBI.dataIdentifiers[0x061F] = "Sensor_for_anti_theft_alarm_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0620] = "Rear_lid_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0621] = "Alarm_horn_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0622] = "Automatic_day_night_interior_mirror_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0623] = "Sun_roof_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0624] = "Steering_column_lock_actuator_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0625] = "Anti_theft_tilt_system_control_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0626] = "Tire_pressure_monitor_antenna_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0627] = "Heated_windshield_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0628] = "Rear_light_left_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0629] = "Ceiling_light_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x062A
] = "Left_front_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x062B
] = "Right_front_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x062C
] = "Control_module_for_auxiliary_air_heater_Coding_Values"
UDS_RDBI.dataIdentifiers[0x062D] = "Ioniser_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x062E
] = "Multi_function_steering_wheel_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x062F] = "Left_rear_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0630] = "Right_rear_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0631] = "Left_rear_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x0632
] = "Right_rear_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0633] = "Display_unit_1_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0634] = "Battery_monitoring_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0635] = "Roof_blind_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0636] = "Sun_roof_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0637] = "Display_unit_2_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0638] = "Telephone_handset_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0639] = "Traffic_data_aerial_Coding_Values"
UDS_RDBI.dataIdentifiers[0x063A] = "Chip_card_reader_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x063B] = "Hands_free_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x063C] = "Telephone_handset_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x063D
] = "Display_unit_front_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x063E] = "Multimedia_operating_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x063F] = "Digital_sound_system_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x0640] = "Control_unit_for_wiper_motor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0641] = "Rain_light_recognition_sensor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0642] = "Light_switch_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0643] = "Garage_door_opener_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0644] = "Garage_door_opener_operating_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0645] = "Ignition_key_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0646
] = "Left_front_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0647
] = "Right_front_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0648
] = "Left_rear_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0649
] = "Right_rear_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x064A] = "Data_medium_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x064B] = "Drivers_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x064C
] = "Front_passengers_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x064D] = "Left_headlamp_power_output_stage_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x064E] = "Right_headlamp_power_output_stage_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x064F
] = "Sensor_for_anti_theft_alarm_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0650] = "Rear_lid_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0651] = "Alarm_horn_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0652
] = "Automatic_day_night_interior_mirror_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0653] = "Sun_roof_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0654] = "Steering_column_lock_actuator_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0655
] = "Anti_theft_tilt_system_control_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0656] = "Tire_pressure_monitor_antenna_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0657] = "Heated_windshield_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0658] = "Rear_light_left_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0659] = "Ceiling_light_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x065A
] = "Left_front_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x065B
] = "Right_front_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x065C
] = "Control_module_for_auxiliary_air_heater_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x065D] = "Ioniser_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x065E
] = "Multi_function_steering_wheel_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x065F] = "Left_rear_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0660] = "Right_rear_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0661
] = "Left_rear_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0662
] = "Right_rear_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0663
] = "Display_unit_1_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0664] = "Battery_monitoring_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0665] = "Roof_blind_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0666] = "Sun_roof_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0667
] = "Display_unit_2_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0668] = "Telephone_handset_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x0669] = "Traffic_data_aerial_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x066A] = "Chip_card_reader_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x066B] = "Hands_free_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x066C] = "Telephone_handset_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x066D
] = "Display_unit_front_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x066E] = "Multimedia_operating_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x066F
] = "Digital_sound_system_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x0670
] = "Control_unit_for_wiper_motor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0671
] = "Rain_light_recognition_sensor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x0672] = "Light_switch_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0673
] = "Garage_door_opener_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0674
] = "Garage_door_opener_operating_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x0675] = "Ignition_key_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0676
] = "Left_front_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0677
] = "Right_front_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0678
] = "Left_rear_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0679
] = "Right_rear_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x067A] = "Data_medium_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x067B
] = "Drivers_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x067C
] = "Front_passengers_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x067D
] = "Left_headlamp_power_output_stage_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x067E
] = "Right_headlamp_power_output_stage_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x067F
] = "Sensor_for_anti_theft_alarm_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0680
] = "Rear_lid_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x0681] = "Alarm_horn_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0682
] = "Automatic_day_night_interior_mirror_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x0683] = "Sun_roof_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0684
] = "Steering_column_lock_actuator_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0685
] = "Anti_theft_tilt_system_control_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0686
] = "Tire_pressure_monitor_antenna_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0687
] = "Heated_windshield_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0688
] = "Rear_light_left_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0689
] = "Ceiling_light_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x068A
] = "Left_front_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x068B
] = "Right_front_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x068C
] = "Control_module_for_auxiliary_air_heater_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x068D] = "Ioniser_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x068E
] = "Multi_function_steering_wheel_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x068F
] = "Left_rear_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0690
] = "Right_rear_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0691
] = "Left_rear_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0692
] = "Right_rear_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0693
] = "Display_unit_1_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0694
] = "Battery_monitoring_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x0695] = "Roof_blind_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x0696] = "Sun_roof_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0697
] = "Display_unit_2_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0698
] = "Telephone_handset_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x0699
] = "Traffic_data_aerial_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x069A
] = "Chip_card_reader_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x069B
] = "Hands_free_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x069C
] = "Telephone_handset_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x069D
] = "Display_unit_front_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x069E
] = "Multimedia_operating_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x069F
] = "Digital_sound_system_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x06A0] = "Control_unit_for_wiper_motor_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06A1] = "Rain_light_recognition_sensor_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06A2] = "Light_switch_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06A3] = "Garage_door_opener_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06A4] = "Garage_door_opener_operating_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06A5] = "Ignition_key_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06A6
] = "Left_front_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06A7
] = "Right_front_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06A8
] = "Left_rear_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06A9
] = "Right_rear_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06AA] = "Data_medium_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06AB] = "Drivers_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06AC
] = "Front_passengers_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06AD] = "Left_headlamp_power_output_stage_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06AE] = "Right_headlamp_power_output_stage_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06AF] = "Sensor_for_anti_theft_alarm_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B0] = "Rear_lid_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B1] = "Alarm_horn_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B2] = "Automatic_day_night_interior_mirror_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B3] = "Sun_roof_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B4] = "Steering_column_lock_actuator_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B5] = "Anti_theft_tilt_system_control_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B6] = "Tire_pressure_monitor_antenna_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B7] = "Heated_windshield_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B8] = "Rear_light_left_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06B9] = "Ceiling_light_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06BA
] = "Left_front_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06BB
] = "Right_front_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06BC
] = "Control_module_for_auxiliary_air_heater_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06BD] = "Ioniser_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06BE
] = "Multi_function_steering_wheel_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06BF] = "Left_rear_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06C0] = "Right_rear_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06C1
] = "Left_rear_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06C2
] = "Right_rear_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06C3
] = "Display_unit_1_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06C4] = "Battery_monitoring_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06C5] = "Roof_blind_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06C6] = "Sun_roof_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06C7
] = "Display_unit_2_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06C8] = "Telephone_handset_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06C9] = "Traffic_data_aerial_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06CA] = "Chip_card_reader_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06CB] = "Hands_free_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06CC] = "Telephone_handset_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06CD
] = "Display_unit_front_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x06CE] = "Multimedia_operating_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06CF
] = "Digital_sound_system_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x06D0
] = "Control_unit_for_wiper_motor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D1
] = "Rain_light_recognition_sensor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06D2] = "Light_switch_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D3
] = "Garage_door_opener_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D4
] = "Garage_door_opener_operating_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06D5] = "Ignition_key_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D6
] = "Left_front_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D7
] = "Right_front_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D8
] = "Left_rear_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06D9
] = "Right_rear_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06DA] = "Data_medium_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06DB] = "Drivers_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06DC
] = "Front_passengers_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06DD
] = "Left_headlamp_power_output_stage_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06DE
] = "Right_headlamp_power_output_stage_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06DF
] = "Sensor_for_anti_theft_alarm_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06E0] = "Rear_lid_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06E1] = "Alarm_horn_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06E2
] = "Automatic_day_night_interior_mirror_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06E3] = "Sun_roof_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06E4
] = "Steering_column_lock_actuator_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06E5
] = "Anti_theft_tilt_system_control_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06E6
] = "Tire_pressure_monitor_antenna_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06E7
] = "Heated_windshield_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06E8] = "Rear_light_left_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06E9] = "Ceiling_light_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06EA
] = "Left_front_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06EB
] = "Right_front_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06EC
] = "Control_module_for_auxiliary_air_heater_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06ED] = "Ioniser_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06EE
] = "Multi_function_steering_wheel_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06EF
] = "Left_rear_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06F0
] = "Right_rear_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06F1
] = "Left_rear_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06F2
] = "Right_rear_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06F3
] = "Display_unit_1_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06F4
] = "Battery_monitoring_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06F5] = "Roof_blind_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06F6] = "Sun_roof_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06F7
] = "Display_unit_2_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06F8] = "Telephone_handset_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06F9] = "Traffic_data_aerial_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06FA
] = "Chip_card_reader_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06FB] = "Hands_free_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06FC] = "Telephone_handset_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06FD
] = "Display_unit_front_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x06FE] = "Multimedia_operating_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x06FF
] = "Digital_sound_system_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x0700] = "Control_unit_for_wiper_motor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0701] = "Rain_light_recognition_sensor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0702] = "Light_switch_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0703] = "Garage_door_opener_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0704] = "Garage_door_opener_operating_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0705] = "Ignition_key_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x0706
] = "Left_front_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x0707
] = "Right_front_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x0708
] = "Left_rear_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x0709
] = "Right_rear_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x070A] = "Data_medium_Serial_Number"
UDS_RDBI.dataIdentifiers[0x070B] = "Drivers_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x070C] = "Front_passengers_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x070D] = "Left_headlamp_power_output_stage_Serial_Number"
UDS_RDBI.dataIdentifiers[0x070E] = "Right_headlamp_power_output_stage_Serial_Number"
UDS_RDBI.dataIdentifiers[0x070F] = "Sensor_for_anti_theft_alarm_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0710] = "Rear_lid_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0711] = "Alarm_horn_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0712] = "Automatic_day_night_interior_mirror_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0713] = "Sun_roof_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0714] = "Steering_column_lock_actuator_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0715] = "Anti_theft_tilt_system_control_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0716] = "Tire_pressure_monitor_antenna_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0717] = "Heated_windshield_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0718] = "Rear_light_left_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0719] = "Ceiling_light_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x071A
] = "Left_front_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x071B
] = "Right_front_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x071C
] = "Control_module_for_auxiliary_air_heater_Serial_Number"
UDS_RDBI.dataIdentifiers[0x071D] = "Ioniser_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x071E
] = "Multi_function_steering_wheel_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x071F] = "Left_rear_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0720] = "Right_rear_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0721] = "Left_rear_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x0722
] = "Right_rear_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0723] = "Display_unit_1_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0724] = "Battery_monitoring_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0725] = "Roof_blind_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0726] = "Sun_roof_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0727] = "Display_unit_2_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0728] = "Telephone_handset_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0729] = "Traffic_data_aerial_Serial_Number"
UDS_RDBI.dataIdentifiers[0x072A] = "Chip_card_reader_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x072B] = "Hands_free_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x072C] = "Telephone_handset_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x072D
] = "Display_unit_front_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x072E] = "Multimedia_operating_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x072F] = "Digital_sound_system_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x0730] = "Control_unit_for_wiper_motor_System_Name"
UDS_RDBI.dataIdentifiers[0x0731] = "Rain_light_recognition_sensor_System_Name"
UDS_RDBI.dataIdentifiers[0x0732] = "Light_switch_System_Name"
UDS_RDBI.dataIdentifiers[0x0733] = "Garage_door_opener_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0734] = "Garage_door_opener_operating_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x0735] = "Ignition_key_System_Name"
UDS_RDBI.dataIdentifiers[
    0x0736
] = "Left_front_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[
    0x0737
] = "Right_front_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[
    0x0738
] = "Left_rear_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[
    0x0739
] = "Right_rear_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x073A] = "Data_medium_System_Name"
UDS_RDBI.dataIdentifiers[0x073B] = "Drivers_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x073C] = "Front_passengers_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x073D] = "Left_headlamp_power_output_stage_System_Name"
UDS_RDBI.dataIdentifiers[0x073E] = "Right_headlamp_power_output_stage_System_Name"
UDS_RDBI.dataIdentifiers[0x073F] = "Sensor_for_anti_theft_alarm_system_System_Name"
UDS_RDBI.dataIdentifiers[0x0740] = "Rear_lid_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x0741] = "Alarm_horn_System_Name"
UDS_RDBI.dataIdentifiers[0x0742] = "Automatic_day_night_interior_mirror_System_Name"
UDS_RDBI.dataIdentifiers[0x0743] = "Sun_roof_System_Name"
UDS_RDBI.dataIdentifiers[0x0744] = "Steering_column_lock_actuator_System_Name"
UDS_RDBI.dataIdentifiers[0x0745] = "Anti_theft_tilt_system_control_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x0746] = "Tire_pressure_monitor_antenna_System_Name"
UDS_RDBI.dataIdentifiers[0x0747] = "Heated_windshield_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0748] = "Rear_light_left_1_System_Name"
UDS_RDBI.dataIdentifiers[0x0749] = "Ceiling_light_module_System_Name"
UDS_RDBI.dataIdentifiers[0x074A] = "Left_front_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x074B] = "Right_front_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x074C] = "Control_module_for_auxiliary_air_heater_System_Name"
UDS_RDBI.dataIdentifiers[0x074D] = "Ioniser_System_Name"
UDS_RDBI.dataIdentifiers[
    0x074E
] = "Multi_function_steering_wheel_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x074F] = "Left_rear_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0750] = "Right_rear_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0751] = "Left_rear_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0752] = "Right_rear_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0753] = "Display_unit_1_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x0754] = "Battery_monitoring_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x0755] = "Roof_blind_System_Name"
UDS_RDBI.dataIdentifiers[0x0756] = "Sun_roof_2_System_Name"
UDS_RDBI.dataIdentifiers[0x0757] = "Display_unit_2_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x0758] = "Telephone_handset_2_System_Name"
UDS_RDBI.dataIdentifiers[0x0759] = "Traffic_data_aerial_System_Name"
UDS_RDBI.dataIdentifiers[0x075A] = "Chip_card_reader_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x075B] = "Hands_free_system_System_Name"
UDS_RDBI.dataIdentifiers[0x075C] = "Telephone_handset_System_Name"
UDS_RDBI.dataIdentifiers[
    0x075D
] = "Display_unit_front_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x075E] = "Multimedia_operating_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x075F] = "Digital_sound_system_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x07A0] = "Control_unit_for_wiper_motor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07A1] = "Rain_light_recognition_sensor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07A2] = "Light_switch_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07A3
] = "Garage_door_opener_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07A4
] = "Garage_door_opener_operating_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07A5] = "Ignition_key_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07A6
] = "Left_front_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07A7
] = "Right_front_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07A8
] = "Left_rear_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07A9
] = "Right_rear_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07AA] = "Data_medium_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07AB] = "Drivers_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07AC
] = "Front_passengers_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07AD
] = "Left_headlamp_power_output_stage_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07AE
] = "Right_headlamp_power_output_stage_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07AF
] = "Sensor_for_anti_theft_alarm_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B0] = "Rear_lid_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B1] = "Alarm_horn_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07B2
] = "Automatic_day_night_interior_mirror_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B3] = "Sun_roof_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B4] = "Steering_column_lock_actuator_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07B5
] = "Anti_theft_tilt_system_control_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B6] = "Tire_pressure_monitor_antenna_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07B7
] = "Heated_windshield_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B8] = "Rear_light_left_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07B9] = "Ceiling_light_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07BA
] = "Left_front_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07BB
] = "Right_front_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07BC
] = "Control_module_for_auxiliary_air_heater_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07BD] = "Ioniser_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07BE
] = "Multi_function_steering_wheel_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07BF] = "Left_rear_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07C0
] = "Right_rear_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07C1
] = "Left_rear_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07C2
] = "Right_rear_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07C3
] = "Display_unit_1_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07C4
] = "Battery_monitoring_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07C5] = "Roof_blind_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07C6] = "Sun_roof_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07C7
] = "Display_unit_2_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07C8] = "Telephone_handset_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07C9] = "Traffic_data_aerial_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07CA
] = "Chip_card_reader_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07CB] = "Hands_free_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07CC] = "Telephone_handset_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07CD
] = "Display_unit_front_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x07CE] = "Multimedia_operating_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x07CF
] = "Digital_sound_system_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x0902] = "Activation of Development CAN-Messages"
UDS_RDBI.dataIdentifiers[0x2A26] = "Gateway Component List present"
UDS_RDBI.dataIdentifiers[0x2A27] = "Gateway_Component_List_Sleepindication"
UDS_RDBI.dataIdentifiers[0x2A28] = "Gateway Component List dtc"
UDS_RDBI.dataIdentifiers[0x2A29] = "Gateway Component List DiagProt"
UDS_RDBI.dataIdentifiers[0x2A2D] = "Gateway_component_list_databus_identification"
UDS_RDBI.dataIdentifiers[0x2EE0] = "Gateway_component_list_diag_path"
UDS_RDBI.dataIdentifiers[0x2EE1] = "Gateway_component_list_ecu_authentication"
UDS_RDBI.dataIdentifiers[
    0x3610
] = "Electrically_adjustable_steering_column_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x3611
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3612] = "Rear_spoiler_adjustment_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3613] = "Roof_blind_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3614] = "Motor_for_wind_deflector_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3615] = "Voltage_stabilizer_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3616] = "Switch_module_for_driver_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x3617
] = "Switch_module_for_front_passenger_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x3618
] = "Switch_module_for_rear_seat_driver_side_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x3619
] = "Switch_module_for_rear_seat_front_passenger_side_Coding_Values"
UDS_RDBI.dataIdentifiers[0x361A] = "Switch_module_2_for_driver_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x361B
] = "Switch_module_2_for_front_passenger_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x361C
] = "Switch_module_2_for_rear_seat_front_passenger_side_Coding_Values"
UDS_RDBI.dataIdentifiers[0x361D] = "Compact_disc_database_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3629] = "LED_headlamp_powermodule_2_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x362A] = "LED_headlamp_powermodule_2_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x362C] = "Multimedia_operating_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x362E] = "Data_medium_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x362F] = "Analog_clock_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3630] = "Relative_Air_Humidity_Interior_Sender_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3631] = "Sensor_controlled_power_rear_lid_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3632] = "Battery_monitoring_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3633] = "Air_conditioning_compressor_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x3634
] = "Control_module_for_auxiliary_blower_motors_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3635] = "High_beam_powermodule_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3636] = "High_beam_powermodule_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x3637] = "Coolant_heater_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x3640
] = "Electrically_adjustable_steering_column_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3641
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3642] = "Rear_spoiler_adjustment_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3643] = "Roof_blind_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3644] = "Motor_for_wind_deflector_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3645] = "Voltage_stabilizer_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3646] = "Switch_module_for_driver_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3647
] = "Switch_module_for_front_passenger_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3648
] = "Switch_module_for_rear_seat_driver_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3649
] = "Switch_module_for_rear_seat_front_passenger_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x364A] = "Switch_module_2_for_driver_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x364B
] = "Switch_module_2_for_front_passenger_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x364C
] = "Switch_module_2_for_rear_seat_front_passenger_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x364D] = "Compact_disc_database_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3659] = "LED_headlamp_powermodule_2_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x365A] = "LED_headlamp_powermodule_2_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x365C] = "Multimedia_operating_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x365E] = "Data_medium_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x365F] = "Analog_clock_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3660
] = "Relative_Air_Humidity_Interior_Sender_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3661] = "Sensor_controlled_power_rear_lid_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3662
] = "Battery_monitoring_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3663] = "Air_conditioning_compressor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3664
] = "Control_module_for_auxiliary_blower_motors_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3665] = "High_beam_powermodule_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3666] = "High_beam_powermodule_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x3667] = "Coolant_heater_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x3670
] = "Electrically_adjustable_steering_column_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3671
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3672
] = "Rear_spoiler_adjustment_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x3673] = "Roof_blind_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3674
] = "Motor_for_wind_deflector_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3675
] = "Voltage_stabilizer_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3676
] = "Switch_module_for_driver_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3677
] = "Switch_module_for_front_passenger_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3678
] = "Switch_module_for_rear_seat_driver_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3679
] = "Switch_module_for_rear_seat_front_passenger_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x367A
] = "Switch_module_2_for_driver_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x367B
] = "Switch_module_2_for_front_passenger_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x367C
] = "Switch_module_2_for_rear_seat_front_passenger_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x367D
] = "Compact_disc_database_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3689
] = "LED_headlamp_powermodule_2_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x368A
] = "LED_headlamp_powermodule_2_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x368C
] = "Multimedia_operating_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x368E] = "Data_medium_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x368F] = "Analog_clock_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3690
] = "Relative_Air_Humidity_Interior_Sender_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3691
] = "Sensor_controlled_power_rear_lid_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3692
] = "Battery_monitoring_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3693
] = "Air_conditioning_compressor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3694
] = "Control_module_for_auxiliary_blower_motors_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3695
] = "High_beam_powermodule_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3696
] = "High_beam_powermodule_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x3697] = "Coolant_heater_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36A0
] = "Electrically_adjustable_steering_column_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36A1
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36A2] = "Rear_spoiler_adjustment_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36A3] = "Roof_blind_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36A4] = "Motor_for_wind_deflector_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36A5] = "Voltage_stabilizer_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36A6] = "Switch_module_for_driver_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36A7
] = "Switch_module_for_front_passenger_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36A8
] = "Switch_module_for_rear_seat_driver_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36A9
] = "Switch_module_for_rear_seat_front_passenger_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36AA] = "Switch_module_2_for_driver_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36AB
] = "Switch_module_2_for_front_passenger_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36AC
] = "Switch_module_2_for_rear_seat_front_passenger_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36AD] = "Compact_disc_database_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36B9] = "LED_headlamp_powermodule_2_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36BA] = "LED_headlamp_powermodule_2_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36BC] = "Multimedia_operating_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36BE] = "Data_medium_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36BF] = "Analog_clock_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36C0
] = "Relative_Air_Humidity_Interior_Sender_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36C1] = "Sensor_controlled_power_rear_lid_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36C2] = "Battery_monitoring_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36C3] = "Air_conditioning_compressor_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36C4
] = "Control_module_for_auxiliary_blower_motors_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36C5] = "High_beam_powermodule_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36C6] = "High_beam_powermodule_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x36C7] = "Coolant_heater_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x36D0
] = "Electrically_adjustable_steering_column_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36D1
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36D2] = "Rear_spoiler_adjustment_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36D3] = "Roof_blind_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36D4] = "Motor_for_wind_deflector_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36D5] = "Voltage_stabilizer_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36D6
] = "Switch_module_for_driver_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36D7
] = "Switch_module_for_front_passenger_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36D8
] = "Switch_module_for_rear_seat_driver_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36D9
] = "Switch_module_for_rear_seat_front_passenger_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36DA
] = "Switch_module_2_for_driver_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36DB
] = "Switch_module_2_for_front_passenger_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36DC
] = "Switch_module_2_for_rear_seat_front_passenger_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36DD] = "Compact_disc_database_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36E9
] = "LED_headlamp_powermodule_2_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36EA
] = "LED_headlamp_powermodule_2_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36EC] = "Multimedia_operating_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36EE] = "Data_medium_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36EF] = "Analog_clock_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36F0
] = "Relative_Air_Humidity_Interior_Sender_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36F1
] = "Sensor_controlled_power_rear_lid_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36F2
] = "Battery_monitoring_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36F3] = "Air_conditioning_compressor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x36F4
] = "Control_module_for_auxiliary_blower_motors_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36F5] = "High_beam_powermodule_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36F6] = "High_beam_powermodule_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x36F7] = "Coolant_heater_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x3700
] = "Electrically_adjustable_steering_column_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x3701
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3702] = "Rear_spoiler_adjustment_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3703] = "Roof_blind_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3704] = "Motor_for_wind_deflector_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3705] = "Voltage_stabilizer_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3706] = "Switch_module_for_driver_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x3707
] = "Switch_module_for_front_passenger_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x3708
] = "Switch_module_for_rear_seat_driver_side_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x3709
] = "Switch_module_for_rear_seat_front_passenger_side_Serial_Number"
UDS_RDBI.dataIdentifiers[0x370A] = "Switch_module_2_for_driver_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x370B
] = "Switch_module_2_for_front_passenger_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x370C
] = "Switch_module_2_for_rear_seat_front_passenger_side_Serial_Number"
UDS_RDBI.dataIdentifiers[0x370D] = "Compact_disc_database_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3719] = "LED_headlamp_powermodule_2_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x371A] = "LED_headlamp_powermodule_2_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x371C] = "Multimedia_operating_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x371E] = "Data_medium_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x371F] = "Analog_clock_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3720] = "Relative_Air_Humidity_Interior_Sender_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3721] = "Sensor_controlled_power_rear_lid_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3722] = "Battery_monitoring_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3723] = "Air_conditioning_compressor_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x3724
] = "Control_module_for_auxiliary_blower_motors_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3725] = "High_beam_powermodule_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3726] = "High_beam_powermodule_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3727] = "Coolant_heater_Serial_Number"
UDS_RDBI.dataIdentifiers[0x3730] = "Electrically_adjustable_steering_column_System_Name"
UDS_RDBI.dataIdentifiers[
    0x3731
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_System_Name"
UDS_RDBI.dataIdentifiers[0x3732] = "Rear_spoiler_adjustment_System_Name"
UDS_RDBI.dataIdentifiers[0x3733] = "Roof_blind_2_System_Name"
UDS_RDBI.dataIdentifiers[0x3734] = "Motor_for_wind_deflector_System_Name"
UDS_RDBI.dataIdentifiers[0x3735] = "Voltage_stabilizer_System_Name"
UDS_RDBI.dataIdentifiers[0x3736] = "Switch_module_for_driver_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x3737] = "Switch_module_for_front_passenger_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x3738] = "Switch_module_for_rear_seat_driver_side_System_Name"
UDS_RDBI.dataIdentifiers[
    0x3739
] = "Switch_module_for_rear_seat_front_passenger_side_System_Name"
UDS_RDBI.dataIdentifiers[0x373A] = "Switch_module_2_for_driver_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x373B
] = "Switch_module_2_for_front_passenger_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x373C
] = "Switch_module_2_for_rear_seat_front_passenger_side_System_Name"
UDS_RDBI.dataIdentifiers[0x373D] = "Compact_disc_database_System_Name"
UDS_RDBI.dataIdentifiers[0x3749] = "LED_headlamp_powermodule_2_left_System_Name"
UDS_RDBI.dataIdentifiers[0x374A] = "LED_headlamp_powermodule_2_right_System_Name"
UDS_RDBI.dataIdentifiers[0x374C] = "Multimedia_operating_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[0x374E] = "Data_medium_2_System_Name"
UDS_RDBI.dataIdentifiers[0x374F] = "Analog_clock_System_Name"
UDS_RDBI.dataIdentifiers[0x3750] = "Relative_Air_Humidity_Interior_Sender_System_Name"
UDS_RDBI.dataIdentifiers[0x3751] = "Sensor_controlled_power_rear_lid_System_Name"
UDS_RDBI.dataIdentifiers[0x3752] = "Battery_monitoring_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x3753] = "Air_conditioning_compressor_System_Name"
UDS_RDBI.dataIdentifiers[
    0x3754
] = "Control_module_for_auxiliary_blower_motors_System_Name"
UDS_RDBI.dataIdentifiers[0x3755] = "High_beam_powermodule_left_System_Name"
UDS_RDBI.dataIdentifiers[0x3756] = "High_beam_powermodule_right_System_Name"
UDS_RDBI.dataIdentifiers[0x3757] = "Coolant_heater_System_Name"
UDS_RDBI.dataIdentifiers[
    0x37A0
] = "Electrically_adjustable_steering_column_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37A1
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37A2] = "Rear_spoiler_adjustment_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37A3] = "Roof_blind_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37A4] = "Motor_for_wind_deflector_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37A5] = "Voltage_stabilizer_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37A6] = "Switch_module_for_driver_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37A7
] = "Switch_module_for_front_passenger_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37A8
] = "Switch_module_for_rear_seat_driver_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37A9
] = "Switch_module_for_rear_seat_front_passenger_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37AA
] = "Switch_module_2_for_driver_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37AB
] = "Switch_module_2_for_front_passenger_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37AC
] = "Switch_module_2_for_rear_seat_front_passenger_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37AD] = "Compact_disc_database_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37B9
] = "LED_headlamp_powermodule_2_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37BA
] = "LED_headlamp_powermodule_2_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37BC] = "Multimedia_operating_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37BE] = "Data_medium_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37BF] = "Analog_clock_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37C0
] = "Relative_Air_Humidity_Interior_Sender_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37C1
] = "Sensor_controlled_power_rear_lid_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37C2
] = "Battery_monitoring_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37C3] = "Air_conditioning_compressor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x37C4
] = "Control_module_for_auxiliary_blower_motors_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37C5] = "High_beam_powermodule_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37C6] = "High_beam_powermodule_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x37C7] = "Coolant_heater_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x5867] = "In_use_monitor_performance_ratio_1"
UDS_RDBI.dataIdentifiers[0x5868] = "In_use_monitor_performance_ratio_2"
UDS_RDBI.dataIdentifiers[0x5869] = "In_use_monitor_performance_ratio_3"
UDS_RDBI.dataIdentifiers[0x6001] = "Control_unit_for_wiper_motor_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6002] = "Rain_light_recognition_sensor_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6003] = "Light_switch_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6004] = "Garage_door_opener_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6005] = "Garage_door_opener_operating_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6006] = "Ignition_key_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6007
] = "Left_front_seat_ventilation_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6008
] = "Right_front_seat_ventilation_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6009
] = "Left_rear_seat_ventilation_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x600A] = "LED_headlamp_powermodule_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x600B] = "LED_headlamp_powermodule_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x600C] = "LED_headlamp_powermodule_2_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x600D] = "LED_headlamp_powermodule_2_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x600E] = "Operating_and_display_unit_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x600F] = "Operating_and_display_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6010
] = "Right_rear_seat_ventilation_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6011] = "Data_medium_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6012] = "Drivers_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6013] = "Front_passengers_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6014] = "Left_headlamp_power_output_stage_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6015] = "Right_headlamp_power_output_stage_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6016] = "Sensor_for_anti_theft_alarm_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6017] = "Rear_lid_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6018] = "Alarm_horn_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6019] = "Automatic_day_night_interior_mirror_Coding_Values"
UDS_RDBI.dataIdentifiers[0x601A] = "Remote_control_auxiliary_heater_Coding_Values"
UDS_RDBI.dataIdentifiers[0x601B] = "Fresh_air_blower_front_Coding_Values"
UDS_RDBI.dataIdentifiers[0x601C] = "Fresh_air_blower_back_Coding_Values"
UDS_RDBI.dataIdentifiers[0x601D] = "Alternator_Coding_Values"
UDS_RDBI.dataIdentifiers[0x601E] = "Interior_light_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x601F
] = "Refrigerant_pressure_and_temperature_sender_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6020] = "Sun_roof_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6021] = "Steering_column_lock_actuator_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6022] = "Anti_theft_tilt_system_control_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6023] = "Tire_pressure_monitor_antenna_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6024] = "Heated_windshield_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6025] = "Rear_light_left_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6026] = "Ceiling_light_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6027
] = "Left_front_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6028
] = "Right_front_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6029
] = "Control_module_for_auxiliary_air_heater_Coding_Values"
UDS_RDBI.dataIdentifiers[0x602A] = "Belt Pretensioner left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x602B] = "Belt Pretensioner right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x602C] = "Occupant Detection_Coding_Values"
UDS_RDBI.dataIdentifiers[0x602D] = "Selector_lever_Coding_Values"
UDS_RDBI.dataIdentifiers[0x602E] = "NOx_sensor_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x602F] = "NOx_sensor_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6030] = "Ioniser_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6031
] = "Multi_function_steering_wheel_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6032] = "Left_rear_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6033] = "Right_rear_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6034] = "Left_rear_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6035
] = "Right_rear_massage_seat_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6036] = "Display_unit_1_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6037] = "Battery_monitoring_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6038] = "Roof_blind_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6039] = "Sun_roof_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x603A] = "Steering_angle_sender_Coding_Values"
UDS_RDBI.dataIdentifiers[0x603B] = "Lane_change_assistant 2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x603C] = "Pitch_rate_sender_Coding_Values"
UDS_RDBI.dataIdentifiers[0x603D] = "ESP_sensor_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x603E] = "Electronic_ignition_lock_Coding_Values"
UDS_RDBI.dataIdentifiers[0x603F] = "Air_quality_sensor_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6040] = "Display_unit_2_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6041] = "Telephone_handset_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6042] = "Chip_card_reader_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6043] = "Traffic_data_aerial_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6044] = "Hands_free_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6045] = "Telephone_handset_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6046
] = "Display_unit_front_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6047] = "Multimedia_operating_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6048] = "Digital_sound_system_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6049
] = "Electrically_adjustable_steering_column_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x604A
] = "Interface_for_external_multimedia_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x604B] = "Relative_Air_Humidity_Interior_Sender_Coding_Values"
UDS_RDBI.dataIdentifiers[0x604C] = "Drivers_door_rear_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x604D] = "Passengers_rear_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x604E] = "Sensor_controlled_power_rear_lid_Coding_Values"
UDS_RDBI.dataIdentifiers[0x604F] = "Camera_for_night_vision_system_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6050
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6051] = "Rear_spoiler_adjustment_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6052] = "Roof_blind_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6053] = "Motor_for_wind_deflector_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6054] = "Voltage_stabilizer_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6055] = "Switch_module_for_driver_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6056
] = "Switch_module_for_front_passenger_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6057
] = "Switch_module_for_rear_seat_driver_side_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6058
] = "Switch_module_for_rear_seat_front_passenger_side_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6059] = "Switch_module_2_for_driver_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[0x605A] = "Battery_charger_unit_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x605B] = "Battery_charger_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x605C] = "Battery_charger_unit_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x605D] = "Air_conditioning_compressor_Coding_Values"
UDS_RDBI.dataIdentifiers[0x605E] = "Neck_heating_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x605F] = "Neck_heating_right_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6060
] = "Switch_module_2_for_front_passenger_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6061
] = "Switch_module_2_for_rear_seat_front_passenger_side_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6062] = "Compact_disc_database_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6063
] = "Rear_climatronic_operating_and_display_unit_left_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6064
] = "Rear_climatronic_operating_and_display_unit_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6065] = "Door_handle_front_left_Kessy_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6066] = "Door_handle_front_right_Kessy_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6067] = "Door_handle_rear_left_Kessy_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6068] = "Door_handle_rear_right_Kessy_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6069] = "Power_converter_DC_AC_Coding_Values"
UDS_RDBI.dataIdentifiers[0x606A] = "Battery_monitoring_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x606B] = "Matrix_headlamp_powermodule_1_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x606C] = "Matrix_headlamp_powermodule_1_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x606D] = "High_beam_powermodule_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x606E] = "High_beam_powermodule_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x606F] = "Air_suspension_compressor_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6070] = "Rear_brake_actuator_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6071] = "Rear_brake_actuator_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6072] = "Analog_clock_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6073] = "Rear_door_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6079] = "Data_medium_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x607A] = "Operating_unit_center_console_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x607B] = "Operating_unit_center_console_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x607C] = "Operating_unit_center_console_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x607D] = "Operating_unit_center_console_4_Coding_Values"
UDS_RDBI.dataIdentifiers[0x607E] = "Interface_for_radiodisplay_Coding_Values"
UDS_RDBI.dataIdentifiers[0x607F] = "Parkassist_entry_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6086] = "Belt_pretensioner_3rd_row_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6087] = "Belt_pretensioner_3rd_row_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6088] = "Injection_valve_heater_control_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6089] = "Steering_column_switch_Coding_Values"
UDS_RDBI.dataIdentifiers[0x608A] = "Brake_assistance_Coding_Values"
UDS_RDBI.dataIdentifiers[0x608B] = "Trailer_articulation_angle_sensor_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x608C
] = "Cup_holder_with_heater_and_cooling_element_Coding_Values"
UDS_RDBI.dataIdentifiers[0x608D] = "Range_of_vision_sensing_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x608E
] = "Convenience_and_driver_assist_operating_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x608F
] = "Cradle_rear_climatronic_operating_and_display_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6090] = "Trailer_weight_nose_weight_detection_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6091] = "Sensor_carbon_dioxide_concentration_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6092] = "Sensor_fine_dust_concentration_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6093] = "Volume_control_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6094] = "Belt_buckle_presenter_2nd_row_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6095] = "Belt_buckle_presenter_2nd_row_right_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6096
] = "Operating_and_display_unit_6_for_air_conditioning_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6097] = "Active_accelerator_pedal_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6098] = "Multimedia_operating_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6099] = "Display_unit_3_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x609A] = "Display_unit_4_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x609B] = "Display_unit_5_for_multimedia_system_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x609C
] = "Control_module_for_auxiliary_blower_motors_Coding_Values"
UDS_RDBI.dataIdentifiers[0x609D] = "Operating_and_display_unit_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x609E] = "Operating_and_display_unit_4_Coding_Values"
UDS_RDBI.dataIdentifiers[0x609F] = "Operating_and_display_unit_5_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A0] = "Side Sensor Driver Front_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A1] = "Side Sensor Passenger Front_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A2] = "Side Sensor Driver Rear_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A3] = "Side Sensor Passenger Rear_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A4] = "Front Sensor Driver_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A5] = "Front Sensor Passenger_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A6] = "Pedestrian Protection Driver_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A7] = "Pedestrian Protection Passenger_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A8] = "Rear Sensor Center_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60A9] = "Pedestrian Protection Center_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60AA] = "Pedestrian Protection Contact_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60AB] = "Pedestrian_protection_driver_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60AC] = "Pedestrian_protection_passenger_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60AD] = "Central_sensor_XY_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60AE
] = "Refrigerant_pressure_and_temperature_sender_2_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60AF
] = "Refrigerant_pressure_and_temperature_sender_3_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B0
] = "Switch_for_rear_multicontour_seat_driver_side_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B1
] = "Valve_block_1_in_driver_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B2
] = "Valve_block_2_in_driver_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B3
] = "Valve_block_3_in_driver_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B4
] = "Switch_for_rear_multicontour_seat_passenger_side_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B5
] = "Valve_block_1_in_passenger_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B6
] = "Valve_block_2_in_passenger_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B7
] = "Valve_block_3_in_passenger_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B8
] = "Switch_for_front_multicontour_seat_driver_side_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60B9
] = "Valve_block_1_in_driver_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60BA
] = "Valve_block_2_in_driver_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60BB
] = "Valve_block_3_in_driver_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60BC
] = "Switch_for_front_multicontour_seat_passenger_side_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60BD
] = "Valve_block_1_in_passenger_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60BE
] = "Valve_block_2_in_passenger_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60BF
] = "Valve_block_3_in_passenger_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C0] = "Coolant_heater_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C1] = "Seat_backrest_fan_1_front_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C2] = "Seat_backrest_fan_2_front_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C3] = "Seat_cushion_fan_1_front_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C4] = "Seat_cushion_fan_2_front_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C5] = "Seat_backrest_fan_1_front_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C6] = "Seat_backrest_fan_2_front_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C7] = "Seat_cushion_fan_1_front_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60C8] = "Seat_cushion_fan_2_front_right_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60C9
] = "Operating_and_display_unit_1_for_air_conditioning_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60CA
] = "Operating_and_display_unit_2_for_air_conditioning_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60CB
] = "Operating_and_display_unit_3_for_air_conditioning_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60CC
] = "Operating_and_display_unit_4_for_air_conditioning_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60CD
] = "Operating_and_display_unit_5_for_air_conditioning_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60CE] = "Pedestrian_protection_left_hand_side_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60CF] = "Pedestrian_protection_right_hand_side_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D0] = "Battery_junction_box_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D1] = "Cell_module_controller_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D2] = "Cell_module_controller_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D3] = "Cell_module_controller_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D4] = "Cell_module_controller_4_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D5] = "Cell_module_controller_5_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D6] = "Cell_module_controller_6_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D7] = "Cell_module_controller_7_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D8] = "Cell_module_controller_8_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60D9] = "Cell_module_controller_9_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60DA] = "Cell_module_controller_10_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60DB] = "Cell_module_controller_11_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60DC] = "Cell_module_controller_12_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60DD] = "Seat_backrest_fan_1_rear_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60DE] = "Seat_backrest_fan_2_rear_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60DF] = "Seat_cushion_fan_1_rear_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E0] = "Seat_cushion_fan_2_rear_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E1] = "Seat_backrest_fan_1_rear_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E2] = "Seat_backrest_fan_2_rear_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E3] = "Seat_cushion_fan_1_rear_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E4] = "Seat_cushion_fan_2_rear_right_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E5] = "Auxiliary_blower_motor_control_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E6] = "Auxiliary_blower_motor_control_2_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60E7
] = "Infrared_sender_for_front_observation_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E8] = "Starter_generator_control_module_sub_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60E9] = "Media_player_1_sub_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60EA] = "Media_player_2_sub_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60EB
] = "Dedicated_short_range_communication_aerial_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60EC
] = "Refrigerant_pressure_and_temperature_sender_4_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60ED
] = "Refrigerant_pressure_and_temperature_sender_5_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60EE
] = "Refrigerant_pressure_and_temperature_sender_6_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60EF] = "Air_coolant_actuator_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F0] = "Air_coolant_actuator_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F1] = "Cell_module_controller_13_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F2] = "Cell_module_controller_14_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F3] = "Cell_module_controller_15_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F5] = "Seat_heating_rear_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F6] = "LED_warning_indicator_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F7] = "Automatic_transmission_fluid_pump_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60F8] = "Manual_transmission_fluid_pump_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60F9
] = "Convenience_and_driver_assist_operating_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x60FB] = "Air_coolant_actuator_3_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60FC
] = "Valve_block_4_in_driver_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60FD
] = "Valve_block_4_in_passenger_side_rear_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60FE
] = "Valve_block_4_in_driver_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x60FF
] = "Valve_block_4_in_passenger_side_front_seat_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6101
] = "Rear_climatronic_operating_and_display_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6102] = "Refrigerant_expansion_valve_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6103] = "Refrigerant_expansion_valve_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6104] = "Refrigerant_expansion_valve_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6105] = "Refrigerant_shut_off_valve_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6106] = "Refrigerant_shut_off_valve_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6107] = "Refrigerant_shut_off_valve_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6108] = "Refrigerant_shut_off_valve_4_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6109] = "Refrigerant_shut_off_valve_5_Coding_Values"
UDS_RDBI.dataIdentifiers[0x610A] = "Sunlight_sensor_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x610B
] = "Near_field_communication_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x610C] = "Clutch_control_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x610D] = "Electrical_charger_Coding_Values"
UDS_RDBI.dataIdentifiers[0x610E] = "Rear_light_left_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x610F] = "Rear_light_right_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6110] = "Rear_light_right_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6111] = "Sunlight_sensor_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6112] = "Radiator_shutter_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6113] = "Radiator_shutter_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6114] = "Radiator_shutter_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6115] = "Radiator_shutter_4_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6118] = "Special_key_operating_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6119] = "Radio_interface_Coding_Values"
UDS_RDBI.dataIdentifiers[0x611A] = "Video_self_protection_recorder_Coding_Values"
UDS_RDBI.dataIdentifiers[0x611B] = "Special_vehicle_assist_interface_Coding_Values"
UDS_RDBI.dataIdentifiers[0x611C] = "Electric_system_disconnection_diode_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x611D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x611E] = "Belt_pretensioner_2nd_row_left_Coding_Values"
UDS_RDBI.dataIdentifiers[0x611F] = "Belt_pretensioner_2nd_row_right_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6120
] = "Electrical_variable_camshaft_phasing_1_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6121
] = "Electrical_variable_camshaft_phasing_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6122] = "Wireless_operating_unit_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6123] = "Wireless_operating_unit_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6124] = "Front_windshield_washer_pump_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6125] = "Air_quality_sensor_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6126] = "Fragrancing_system_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6127] = "Coolant_valve_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6128
] = "Near_field_communication_control_module_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6129] = "Interior_monitoring_rear_Coding_Values"
UDS_RDBI.dataIdentifiers[0x612A] = "Cooler_fan_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x612B] = "Control_unit_heating_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x612C] = "Control_unit_heating_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x612D] = "Control_unit_heating_3_Coding_Values"
UDS_RDBI.dataIdentifiers[0x612E] = "Control_unit_heating_4_Coding_Values"
UDS_RDBI.dataIdentifiers[0x612F] = "Operating_unit_drive_mode_selection_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6130] = "Side_sensor_a-pillar_driver_front_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6131] = "Side_sensor_a-pillar_passenger_front_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6132] = "Sensor_high_voltage_system_1_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6133] = "Side_sensor_b-pillar_driver_front_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6134] = "Side_sensor_b-pillar_passenger_front_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x6135
] = "Multi_function_steering_wheel_control_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6136] = "Gear_selection_display_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6137] = "Cooler_fan_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6138] = "Gear_selector_control_module_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6139] = "Interior_light_module_2_Coding_Values"
UDS_RDBI.dataIdentifiers[0x613A] = "Radio_control_center_Coding_Values"
UDS_RDBI.dataIdentifiers[0x613B] = "Multimedia_extension_Coding_Values"
UDS_RDBI.dataIdentifiers[0x613C] = "Control_unit_differential_lock_Coding_Values"
UDS_RDBI.dataIdentifiers[0x613D] = "Control_unit_ride_control_system_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x613E
] = "Control_unit_hands_on_detection_steering_wheel_Coding_Values"
UDS_RDBI.dataIdentifiers[
    0x613F
] = "Front_climatronic_operating_and_display_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6140] = "Auxiliary_display_unit_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6141] = "Card_reader_tv_tuner_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6142] = "Park_lock_actuator_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6143] = "Media_connector_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6144] = "Catalyst_heating_Coding_Values"
UDS_RDBI.dataIdentifiers[0x6201] = "Control_unit_for_wiper_motor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6202] = "Rain_light_recognition_sensor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6203] = "Light_switch_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6204] = "Garage_door_opener_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6205] = "Garage_door_opener_operating_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6206] = "Ignition_key_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6207
] = "Left_front_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6208
] = "Right_front_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6209
] = "Left_rear_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x620A] = "LED_headlamp_powermodule_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x620B] = "LED_headlamp_powermodule_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x620C] = "LED_headlamp_powermodule_2_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x620D] = "LED_headlamp_powermodule_2_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x620E] = "Operating_and_display_unit_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x620F] = "Operating_and_display_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6210
] = "Right_rear_seat_ventilation_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6211] = "Data_medium_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6212] = "Drivers_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6213
] = "Front_passengers_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6214] = "Left_headlamp_power_output_stage_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6215] = "Right_headlamp_power_output_stage_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6216
] = "Sensor_for_anti_theft_alarm_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6217] = "Rear_lid_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6218] = "Alarm_horn_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6219
] = "Automatic_day_night_interior_mirror_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x621A] = "Remote_control_auxiliary_heater_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x621B] = "Fresh_air_blower_front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x621C] = "Fresh_air_blower_back_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x621D] = "Alternator_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x621E] = "Interior_light_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x621F
] = "Refrigerant_pressure_and_temperature_sender_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6220] = "Sun_roof_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6221] = "Steering_column_lock_actuator_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6222
] = "Anti_theft_tilt_system_control_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6223] = "Tire_pressure_monitor_antenna_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6224] = "Heated_windshield_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6225] = "Rear_light_left_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6226] = "Ceiling_light_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6227
] = "Left_front_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6228
] = "Right_front_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6229
] = "Control_module_for_auxiliary_air_heater_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x622A] = "Belt Pretensioner left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x622B] = "Belt Pretensioner right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x622C] = "Occupant Detection_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x622D] = "Selector_lever_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x622E] = "NOx_sensor_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x622F] = "NOx_sensor_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6230] = "Ioniser_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6231
] = "Multi_function_steering_wheel_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6232] = "Left_rear_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6233] = "Right_rear_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6234
] = "Left_rear_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6235
] = "Right_rear_massage_seat_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6236
] = "Display_unit_1_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6237] = "Battery_monitoring_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6238] = "Roof_blind_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6239] = "Sun_roof_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x623A] = "Steering_angle_sender_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x623B] = "Lane_change_assistant 2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x623C] = "Pitch_rate_sender_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x623D] = "ESP_sensor_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x623E] = "Electronic_ignition_lock_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x623F] = "Air_quality_sensor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6240
] = "Display_unit_2_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6241] = "Telephone_handset_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6242] = "Chip_card_reader_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6243] = "Traffic_data_aerial_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6244] = "Hands_free_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6245] = "Telephone_handset_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6246
] = "Display_unit_front_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6247] = "Multimedia_operating_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6248
] = "Digital_sound_system_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6249
] = "Electrically_adjustable_steering_column_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x624A
] = "Interface_for_external_multimedia_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x624B
] = "Relative_Air_Humidity_Interior_Sender_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x624C] = "Drivers_door_rear_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x624D
] = "Passengers_rear_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x624E] = "Sensor_controlled_power_rear_lid_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x624F] = "Camera_for_night_vision_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6250
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6251] = "Rear_spoiler_adjustment_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6252] = "Roof_blind_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6253] = "Motor_for_wind_deflector_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6254] = "Voltage_stabilizer_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6255] = "Switch_module_for_driver_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6256
] = "Switch_module_for_front_passenger_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6257
] = "Switch_module_for_rear_seat_driver_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6258
] = "Switch_module_for_rear_seat_front_passenger_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6259] = "Switch_module_2_for_driver_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x625A] = "Battery_charger_unit_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x625B] = "Battery_charger_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x625C] = "Battery_charger_unit_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x625D] = "Air_conditioning_compressor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x625E] = "Neck_heating_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x625F] = "Neck_heating_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6260
] = "Switch_module_2_for_front_passenger_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6261
] = "Switch_module_2_for_rear_seat_front_passenger_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6262] = "Compact_disc_database_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6263
] = "Rear_climatronic_operating_and_display_unit_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6264
] = "Rear_climatronic_operating_and_display_unit_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6265] = "Door_handle_front_left_Kessy_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6266] = "Door_handle_front_right_Kessy_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6267] = "Door_handle_rear_left_Kessy_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6268] = "Door_handle_rear_right_Kessy_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6269] = "Power_converter_DC_AC_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x626A
] = "Battery_monitoring_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x626B
] = "Matrix_headlamp_powermodule_1_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x626C
] = "Matrix_headlamp_powermodule_1_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x626D] = "High_beam_powermodule_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x626E] = "High_beam_powermodule_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x626F] = "Air_suspension_compressor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6270] = "Rear_brake_actuator_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6271] = "Rear_brake_actuator_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6272] = "Analog_clock_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6273] = "Rear_door_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6279] = "Data_medium_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x627A] = "Operating_unit_center_console_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x627B] = "Operating_unit_center_console_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x627C] = "Operating_unit_center_console_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x627D] = "Operating_unit_center_console_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x627E] = "Interface_for_radiodisplay_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x627F] = "Parkassist_entry_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6286] = "Belt_pretensioner_3rd_row_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6287] = "Belt_pretensioner_3rd_row_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6288
] = "Injection_valve_heater_control_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6289] = "Steering_column_switch_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x628A] = "Brake_assistance_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x628B] = "Trailer_articulation_angle_sensor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x628C
] = "Cup_holder_with_heater_and_cooling_element_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x628D] = "Range_of_vision_sensing_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x628E
] = "Convenience_and_driver_assist_operating_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x628F
] = "Cradle_rear_climatronic_operating_and_display_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6290
] = "Trailer_weight_nose_weight_detection_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6291
] = "Sensor_carbon_dioxide_concentration_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6292] = "Sensor_fine_dust_concentration_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6293] = "Volume_control_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6294
] = "Belt_buckle_presenter_2nd_row_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6295
] = "Belt_buckle_presenter_2nd_row_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6296
] = "Operating_and_display_unit_6_for_air_conditioning_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6297] = "Active_accelerator_pedal_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6298] = "Multimedia_operating_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6299
] = "Display_unit_3_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x629A
] = "Display_unit_4_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x629B
] = "Display_unit_5_for_multimedia_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x629C
] = "Control_module_for_auxiliary_blower_motors_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x629D] = "Operating_and_display_unit_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x629E] = "Operating_and_display_unit_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x629F] = "Operating_and_display_unit_5_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A0] = "Side Sensor Driver Front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A1] = "Side Sensor Passenger Front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A2] = "Side Sensor Driver Rear_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A3] = "Side Sensor Passenger Rear_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A4] = "Front Sensor Driver_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A5] = "Front Sensor Passenger_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A6] = "Pedestrian Protection Driver_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A7] = "Pedestrian Protection Passenger_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A8] = "Rear Sensor Center_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62A9] = "Pedestrian Protection Center_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62AA] = "Pedestrian Protection Contact_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62AB] = "Pedestrian_protection_driver_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62AC] = "Pedestrian_protection_passenger_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62AD] = "Central_sensor_XY_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62AE
] = "Refrigerant_pressure_and_temperature_sender_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62AF
] = "Refrigerant_pressure_and_temperature_sender_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B0
] = "Switch_for_rear_multicontour_seat_driver_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B1
] = "Valve_block_1_in_driver_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B2
] = "Valve_block_2_in_driver_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B3
] = "Valve_block_3_in_driver_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B4
] = "Switch_for_rear_multicontour_seat_passenger_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B5
] = "Valve_block_1_in_passenger_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B6
] = "Valve_block_2_in_passenger_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B7
] = "Valve_block_3_in_passenger_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B8
] = "Switch_for_front_multicontour_seat_driver_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62B9
] = "Valve_block_1_in_driver_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62BA
] = "Valve_block_2_in_driver_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62BB
] = "Valve_block_3_in_driver_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62BC
] = "Switch_for_front_multicontour_seat_passenger_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62BD
] = "Valve_block_1_in_passenger_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62BE
] = "Valve_block_2_in_passenger_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62BF
] = "Valve_block_3_in_passenger_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C0] = "Coolant_heater_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C1] = "Seat_backrest_fan_1_front_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C2] = "Seat_backrest_fan_2_front_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C3] = "Seat_cushion_fan_1_front_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C4] = "Seat_cushion_fan_2_front_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C5] = "Seat_backrest_fan_1_front_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C6] = "Seat_backrest_fan_2_front_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C7] = "Seat_cushion_fan_1_front_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62C8] = "Seat_cushion_fan_2_front_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62C9
] = "Operating_and_display_unit_1_for_air_conditioning_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62CA
] = "Operating_and_display_unit_2_for_air_conditioning_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62CB
] = "Operating_and_display_unit_3_for_air_conditioning_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62CC
] = "Operating_and_display_unit_4_for_air_conditioning_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62CD
] = "Operating_and_display_unit_5_for_air_conditioning_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62CE
] = "Pedestrian_protection_left_hand_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62CF
] = "Pedestrian_protection_right_hand_side_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D0] = "Battery_junction_box_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D1] = "Cell_module_controller_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D2] = "Cell_module_controller_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D3] = "Cell_module_controller_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D4] = "Cell_module_controller_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D5] = "Cell_module_controller_5_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D6] = "Cell_module_controller_6_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D7] = "Cell_module_controller_7_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D8] = "Cell_module_controller_8_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62D9] = "Cell_module_controller_9_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62DA] = "Cell_module_controller_10_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62DB] = "Cell_module_controller_11_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62DC] = "Cell_module_controller_12_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62DD] = "Seat_backrest_fan_1_rear_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62DE] = "Seat_backrest_fan_2_rear_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62DF] = "Seat_cushion_fan_1_rear_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E0] = "Seat_cushion_fan_2_rear_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E1] = "Seat_backrest_fan_1_rear_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E2] = "Seat_backrest_fan_2_rear_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E3] = "Seat_cushion_fan_1_rear_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E4] = "Seat_cushion_fan_2_rear_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E5] = "Auxiliary_blower_motor_control_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E6] = "Auxiliary_blower_motor_control_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62E7
] = "Infrared_sender_for_front_observation_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62E8
] = "Starter_generator_control_module_sub_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62E9] = "Media_player_1_sub_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62EA] = "Media_player_2_sub_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62EB
] = "Dedicated_short_range_communication_aerial_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62EC
] = "Refrigerant_pressure_and_temperature_sender_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62ED
] = "Refrigerant_pressure_and_temperature_sender_5_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62EE
] = "Refrigerant_pressure_and_temperature_sender_6_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62EF] = "Air_coolant_actuator_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F0] = "Air_coolant_actuator_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F1] = "Cell_module_controller_13_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F2] = "Cell_module_controller_14_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F3] = "Cell_module_controller_15_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F5] = "Seat_heating_rear_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F6] = "LED_warning_indicator_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F7] = "Automatic_transmission_fluid_pump_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62F8] = "Manual_transmission_fluid_pump_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62F9
] = "Convenience_and_driver_assist_operating_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x62FB] = "Air_coolant_actuator_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62FC
] = "Valve_block_4_in_driver_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62FD
] = "Valve_block_4_in_passenger_side_rear_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62FE
] = "Valve_block_4_in_driver_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x62FF
] = "Valve_block_4_in_passenger_side_front_seat_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6301
] = "Rear_climatronic_operating_and_display_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6302] = "Refrigerant_expansion_valve_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6303] = "Refrigerant_expansion_valve_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6304] = "Refrigerant_expansion_valve_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6305] = "Refrigerant_shut_off_valve_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6306] = "Refrigerant_shut_off_valve_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6307] = "Refrigerant_shut_off_valve_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6308] = "Refrigerant_shut_off_valve_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6309] = "Refrigerant_shut_off_valve_5_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x630A] = "Sunlight_sensor_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x630B
] = "Near_field_communication_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x630C] = "Clutch_control_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x630D] = "Electrical_charger_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x630E] = "Rear_light_left_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x630F] = "Rear_light_right_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6310] = "Rear_light_right_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6311] = "Sunlight_sensor_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6312] = "Radiator_shutter_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6313] = "Radiator_shutter_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6314] = "Radiator_shutter_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6315] = "Radiator_shutter_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6318] = "Special_key_operating_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6319] = "Radio_interface_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x631A] = "Video_self_protection_recorder_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x631B] = "Special_vehicle_assist_interface_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x631C
] = "Electric_system_disconnection_diode_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x631D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x631E] = "Belt_pretensioner_2nd_row_left_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x631F] = "Belt_pretensioner_2nd_row_right_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6320
] = "Electrical_variable_camshaft_phasing_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6321
] = "Electrical_variable_camshaft_phasing_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6322] = "Wireless_operating_unit_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6323] = "Wireless_operating_unit_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6324] = "Front_windshield_washer_pump_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6325] = "Air_quality_sensor_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6326] = "Fragrancing_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6327] = "Coolant_valve_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6328
] = "Near_field_communication_control_module_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6329] = "Interior_monitoring_rear_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x632A] = "Cooler_fan_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x632B] = "Control_unit_heating_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x632C] = "Control_unit_heating_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x632D] = "Control_unit_heating_3_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x632E] = "Control_unit_heating_4_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x632F
] = "Operating_unit_drive_mode_selection_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6330] = "Side_sensor_a-pillar_driver_front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6331
] = "Side_sensor_a-pillar_passenger_front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6332] = "Sensor_high_voltage_system_1_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6333] = "Side_sensor_b-pillar_driver_front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6334
] = "Side_sensor_b-pillar_passenger_front_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6335
] = "Multi_function_steering_wheel_control_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6336] = "Gear_selection_display_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6337] = "Cooler_fan_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6338] = "Gear_selector_control_module_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6339] = "Interior_light_module_2_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x633A] = "Radio_control_center_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x633B] = "Multimedia_extension_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x633C] = "Control_unit_differential_lock_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x633D] = "Control_unit_ride_control_system_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x633E
] = "Control_unit_hands_on_detection_steering_wheel_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x633F
] = "Front_climatronic_operating_and_display_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6340] = "Auxiliary_display_unit_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6341] = "Card_reader_tv_tuner_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6342] = "Park_lock_actuator_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6343] = "Media_connector_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[0x6344] = "Catalyst_heating_Spare_Part_Number"
UDS_RDBI.dataIdentifiers[
    0x6401
] = "Control_unit_for_wiper_motor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6402
] = "Rain_light_recognition_sensor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6403] = "Light_switch_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6404
] = "Garage_door_opener_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6405
] = "Garage_door_opener_operating_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6406] = "Ignition_key_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6407
] = "Left_front_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6408
] = "Right_front_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6409
] = "Left_rear_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x640A
] = "LED_headlamp_powermodule_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x640B
] = "LED_headlamp_powermodule_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x640C
] = "LED_headlamp_powermodule_2_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x640D
] = "LED_headlamp_powermodule_2_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x640E
] = "Operating_and_display_unit_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x640F
] = "Operating_and_display_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6410
] = "Right_rear_seat_ventilation_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6411] = "Data_medium_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6412
] = "Drivers_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6413
] = "Front_passengers_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6414
] = "Left_headlamp_power_output_stage_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6415
] = "Right_headlamp_power_output_stage_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6416
] = "Sensor_for_anti_theft_alarm_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6417
] = "Rear_lid_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6418] = "Alarm_horn_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6419
] = "Automatic_day_night_interior_mirror_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x641A
] = "Remote_control_auxiliary_heater_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x641B
] = "Fresh_air_blower_front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x641C
] = "Fresh_air_blower_back_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x641D] = "Alternator_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x641E
] = "Interior_light_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x641F
] = "Refrigerant_pressure_and_temperature_sender_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6420] = "Sun_roof_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6421
] = "Steering_column_lock_actuator_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6422
] = "Anti_theft_tilt_system_control_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6423
] = "Tire_pressure_monitor_antenna_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6424
] = "Heated_windshield_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6425
] = "Rear_light_left_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6426
] = "Ceiling_light_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6427
] = "Left_front_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6428
] = "Right_front_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6429
] = "Control_module_for_auxiliary_air_heater_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x642A
] = "Belt Pretensioner left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x642B
] = "Belt Pretensioner right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x642C
] = "Occupant Detection_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x642D] = "Selector_lever_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x642E] = "NOx_sensor_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x642F] = "NOx_sensor_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6430] = "Ioniser_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6431
] = "Multi_function_steering_wheel_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6432
] = "Left_rear_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6433
] = "Right_rear_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6434
] = "Left_rear_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6435
] = "Right_rear_massage_seat_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6436
] = "Display_unit_1_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6437
] = "Battery_monitoring_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6438] = "Roof_blind_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6439] = "Sun_roof_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x643A
] = "Steering_angle_sender_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x643B
] = "Lane_change_assistant 2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x643C
] = "Pitch_rate_sender_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x643D] = "ESP_sensor_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x643E
] = "Electronic_ignition_lock_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x643F
] = "Air_quality_sensor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6440
] = "Display_unit_2_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6441
] = "Telephone_handset_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6442
] = "Chip_card_reader_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6443
] = "Traffic_data_aerial_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6444
] = "Hands_free_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6445
] = "Telephone_handset_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6446
] = "Display_unit_front_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6447
] = "Multimedia_operating_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6448
] = "Digital_sound_system_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6449
] = "Electrically_adjustable_steering_column_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x644A
] = "Interface_for_external_multimedia_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x644B
] = "Relative_Air_Humidity_Interior_Sender_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x644C
] = "Drivers_door_rear_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x644D
] = "Passengers_rear_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x644E
] = "Sensor_controlled_power_rear_lid_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x644F
] = "Camera_for_night_vision_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6450
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6451
] = "Rear_spoiler_adjustment_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6452] = "Roof_blind_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6453
] = "Motor_for_wind_deflector_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6454
] = "Voltage_stabilizer_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6455
] = "Switch_module_for_driver_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6456
] = "Switch_module_for_front_passenger_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6457
] = "Switch_module_for_rear_seat_driver_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6458
] = "Switch_module_for_rear_seat_front_passenger_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6459
] = "Switch_module_2_for_driver_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x645A
] = "Battery_charger_unit_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x645B
] = "Battery_charger_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x645C
] = "Battery_charger_unit_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x645D
] = "Air_conditioning_compressor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x645E
] = "Neck_heating_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x645F
] = "Neck_heating_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6460
] = "Switch_module_2_for_front_passenger_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6461
] = "Switch_module_2_for_rear_seat_front_passenger_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6462
] = "Compact_disc_database_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6463
] = "Rear_climatronic_operating_and_display_unit_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6464
] = "Rear_climatronic_operating_and_display_unit_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6465
] = "Door_handle_front_left_Kessy_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6466
] = "Door_handle_front_right_Kessy_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6467
] = "Door_handle_rear_left_Kessy_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6468
] = "Door_handle_rear_right_Kessy_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6469
] = "Power_converter_DC_AC_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x646A
] = "Battery_monitoring_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x646B
] = "Matrix_headlamp_powermodule_1_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x646C
] = "Matrix_headlamp_powermodule_1_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x646D
] = "High_beam_powermodule_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x646E
] = "High_beam_powermodule_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x646F
] = "Air_suspension_compressor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6470
] = "Rear_brake_actuator_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6471
] = "Rear_brake_actuator_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6472] = "Analog_clock_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6473
] = "Rear_door_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6479] = "Data_medium_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x647A
] = "Operating_unit_center_console_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x647B
] = "Operating_unit_center_console_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x647C
] = "Operating_unit_center_console_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x647D
] = "Operating_unit_center_console_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x647E
] = "Interface_for_radiodisplay_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x647F
] = "Parkassist_entry_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6486
] = "Belt_pretensioner_3rd_row_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6487
] = "Belt_pretensioner_3rd_row_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6488
] = "Injection_valve_heater_control_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6489
] = "Steering_column_switch_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x648A
] = "Brake_assistance_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x648B
] = "Trailer_articulation_angle_sensor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x648C
] = "Cup_holder_with_heater_and_cooling_element_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x648D
] = "Range_of_vision_sensing_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x648E
] = "Convenience_and_driver_assist_operating_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x648F
] = "Cradle_rear_climatronic_operating_and_display_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6490
] = "Trailer_weight_nose_weight_detection_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6491
] = "Sensor_carbon_dioxide_concentration_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6492
] = "Sensor_fine_dust_concentration_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6493
] = "Volume_control_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6494
] = "Belt_buckle_presenter_2nd_row_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6495
] = "Belt_buckle_presenter_2nd_row_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6496
] = "Operating_and_display_unit_6_for_air_conditioning_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6497
] = "Active_accelerator_pedal_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6498
] = "Multimedia_operating_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6499
] = "Display_unit_3_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x649A
] = "Display_unit_4_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x649B
] = "Display_unit_5_for_multimedia_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x649C
] = "Control_module_for_auxiliary_blower_motors_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x649D
] = "Operating_and_display_unit_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x649E
] = "Operating_and_display_unit_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x649F
] = "Operating_and_display_unit_5_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A0
] = "Side Sensor Driver Front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A1
] = "Side Sensor Passenger Front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A2
] = "Side Sensor Driver Rear_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A3
] = "Side Sensor Passenger Rear_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A4
] = "Front Sensor Driver_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A5
] = "Front Sensor Passenger_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A6
] = "Pedestrian Protection Driver_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A7
] = "Pedestrian Protection Passenger_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A8
] = "Rear Sensor Center_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64A9
] = "Pedestrian Protection Center_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64AA
] = "Pedestrian Protection Contact_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64AB
] = "Pedestrian_protection_driver_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64AC
] = "Pedestrian_protection_passenger_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64AD
] = "Central_sensor_XY_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64AE
] = "Refrigerant_pressure_and_temperature_sender_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64AF
] = "Refrigerant_pressure_and_temperature_sender_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B0
] = "Switch_for_rear_multicontour_seat_driver_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B1
] = "Valve_block_1_in_driver_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B2
] = "Valve_block_2_in_driver_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B3
] = "Valve_block_3_in_driver_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B4
] = "Switch_for_rear_multicontour_seat_passenger_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B5
] = "Valve_block_1_in_passenger_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B6
] = "Valve_block_2_in_passenger_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B7
] = "Valve_block_3_in_passenger_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B8
] = "Switch_for_front_multicontour_seat_driver_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64B9
] = "Valve_block_1_in_driver_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64BA
] = "Valve_block_2_in_driver_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64BB
] = "Valve_block_3_in_driver_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64BC
] = "Switch_for_front_multicontour_seat_passenger_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64BD
] = "Valve_block_1_in_passenger_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64BE
] = "Valve_block_2_in_passenger_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64BF
] = "Valve_block_3_in_passenger_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x64C0] = "Coolant_heater_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C1
] = "Seat_backrest_fan_1_front_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C2
] = "Seat_backrest_fan_2_front_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C3
] = "Seat_cushion_fan_1_front_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C4
] = "Seat_cushion_fan_2_front_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C5
] = "Seat_backrest_fan_1_front_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C6
] = "Seat_backrest_fan_2_front_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C7
] = "Seat_cushion_fan_1_front_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C8
] = "Seat_cushion_fan_2_front_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64C9
] = "Operating_and_display_unit_1_for_air_conditioning_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64CA
] = "Operating_and_display_unit_2_for_air_conditioning_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64CB
] = "Operating_and_display_unit_3_for_air_conditioning_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64CC
] = "Operating_and_display_unit_4_for_air_conditioning_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64CD
] = "Operating_and_display_unit_5_for_air_conditioning_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64CE
] = "Pedestrian_protection_left_hand_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64CF
] = "Pedestrian_protection_right_hand_side_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D0
] = "Battery_junction_box_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D1
] = "Cell_module_controller_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D2
] = "Cell_module_controller_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D3
] = "Cell_module_controller_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D4
] = "Cell_module_controller_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D5
] = "Cell_module_controller_5_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D6
] = "Cell_module_controller_6_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D7
] = "Cell_module_controller_7_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D8
] = "Cell_module_controller_8_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64D9
] = "Cell_module_controller_9_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64DA
] = "Cell_module_controller_10_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64DB
] = "Cell_module_controller_11_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64DC
] = "Cell_module_controller_12_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64DD
] = "Seat_backrest_fan_1_rear_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64DE
] = "Seat_backrest_fan_2_rear_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64DF
] = "Seat_cushion_fan_1_rear_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E0
] = "Seat_cushion_fan_2_rear_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E1
] = "Seat_backrest_fan_1_rear_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E2
] = "Seat_backrest_fan_2_rear_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E3
] = "Seat_cushion_fan_1_rear_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E4
] = "Seat_cushion_fan_2_rear_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E5
] = "Auxiliary_blower_motor_control_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E6
] = "Auxiliary_blower_motor_control_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E7
] = "Infrared_sender_for_front_observation_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E8
] = "Starter_generator_control_module_sub_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64E9
] = "Media_player_1_sub_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64EA
] = "Media_player_2_sub_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64EB
] = "Dedicated_short_range_communication_aerial_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64EC
] = "Refrigerant_pressure_and_temperature_sender_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64ED
] = "Refrigerant_pressure_and_temperature_sender_5_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64EE
] = "Refrigerant_pressure_and_temperature_sender_6_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64EF
] = "Air_coolant_actuator_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F0
] = "Air_coolant_actuator_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F1
] = "Cell_module_controller_13_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F2
] = "Cell_module_controller_14_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F3
] = "Cell_module_controller_15_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F5
] = "Seat_heating_rear_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F6
] = "LED_warning_indicator_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F7
] = "Automatic_transmission_fluid_pump_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F8
] = "Manual_transmission_fluid_pump_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64F9
] = "Convenience_and_driver_assist_operating_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64FB
] = "Air_coolant_actuator_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64FC
] = "Valve_block_4_in_driver_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64FD
] = "Valve_block_4_in_passenger_side_rear_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64FE
] = "Valve_block_4_in_driver_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x64FF
] = "Valve_block_4_in_passenger_side_front_seat_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6501
] = "Rear_climatronic_operating_and_display_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6502
] = "Refrigerant_expansion_valve_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6503
] = "Refrigerant_expansion_valve_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6504
] = "Refrigerant_expansion_valve_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6505
] = "Refrigerant_shut_off_valve_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6506
] = "Refrigerant_shut_off_valve_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6507
] = "Refrigerant_shut_off_valve_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6508
] = "Refrigerant_shut_off_valve_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6509
] = "Refrigerant_shut_off_valve_5_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x650A] = "Sunlight_sensor_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x650B
] = "Near_field_communication_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x650C
] = "Clutch_control_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x650D
] = "Electrical_charger_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x650E
] = "Rear_light_left_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x650F
] = "Rear_light_right_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6510
] = "Rear_light_right_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6511
] = "Sunlight_sensor_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6512
] = "Radiator_shutter_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6513
] = "Radiator_shutter_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6514
] = "Radiator_shutter_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6515
] = "Radiator_shutter_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6518
] = "Special_key_operating_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6519] = "Radio_interface_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x651A
] = "Video_self_protection_recorder_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x651B
] = "Special_vehicle_assist_interface_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x651C
] = "Electric_system_disconnection_diode_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x651D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x651E
] = "Belt_pretensioner_2nd_row_left_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x651F
] = "Belt_pretensioner_2nd_row_right_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6520
] = "Electrical_variable_camshaft_phasing_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6521
] = "Electrical_variable_camshaft_phasing_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6522
] = "Wireless_operating_unit_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6523
] = "Wireless_operating_unit_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6524
] = "Front_windshield_washer_pump_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6525
] = "Air_quality_sensor_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6526
] = "Fragrancing_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6527] = "Coolant_valve_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6528
] = "Near_field_communication_control_module_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6529
] = "Interior_monitoring_rear_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x652A] = "Cooler_fan_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x652B
] = "Control_unit_heating_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x652C
] = "Control_unit_heating_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x652D
] = "Control_unit_heating_3_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x652E
] = "Control_unit_heating_4_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x652F
] = "Operating_unit_drive_mode_selection_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6530
] = "Side_sensor_a-pillar_driver_front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6531
] = "Side_sensor_a-pillar_passenger_front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6532
] = "Sensor_high_voltage_system_1_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6533
] = "Side_sensor_b-pillar_driver_front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6534
] = "Side_sensor_b-pillar_passenger_front_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6535
] = "Multi_function_steering_wheel_control_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6536
] = "Gear_selection_display_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6537] = "Cooler_fan_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6538
] = "Gear_selector_control_module_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6539
] = "Interior_light_module_2_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x653A
] = "Radio_control_center_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x653B
] = "Multimedia_extension_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x653C
] = "Control_unit_differential_lock_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x653D
] = "Control_unit_ride_control_system_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x653E
] = "Control_unit_hands_on_detection_steering_wheel_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x653F
] = "Front_climatronic_operating_and_display_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6540
] = "Auxiliary_display_unit_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6541
] = "Card_reader_tv_tuner_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6542
] = "Park_lock_actuator_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6543] = "Media_connector_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6544
] = "Catalyst_heating_Application_Software_Version_Number"
UDS_RDBI.dataIdentifiers[0x6601] = "Control_unit_for_wiper_motor_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6602] = "Rain_light_recognition_sensor_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6603] = "Light_switch_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6604] = "Garage_door_opener_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6605] = "Garage_door_opener_operating_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6606] = "Ignition_key_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6607
] = "Left_front_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6608
] = "Right_front_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6609
] = "Left_rear_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x660A] = "LED_headlamp_powermodule_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x660B] = "LED_headlamp_powermodule_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x660C] = "LED_headlamp_powermodule_2_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x660D] = "LED_headlamp_powermodule_2_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x660E] = "Operating_and_display_unit_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x660F] = "Operating_and_display_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6610
] = "Right_rear_seat_ventilation_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6611] = "Data_medium_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6612] = "Drivers_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6613
] = "Front_passengers_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6614] = "Left_headlamp_power_output_stage_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6615] = "Right_headlamp_power_output_stage_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6616] = "Sensor_for_anti_theft_alarm_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6617] = "Rear_lid_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6618] = "Alarm_horn_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6619] = "Automatic_day_night_interior_mirror_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x661A] = "Remote_control_auxiliary_heater_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x661B] = "Fresh_air_blower_front_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x661C] = "Fresh_air_blower_back_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x661D] = "Alternator_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x661E] = "Interior_light_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x661F
] = "Refrigerant_pressure_and_temperature_sender_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6620] = "Sun_roof_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6621] = "Steering_column_lock_actuator_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6622] = "Anti_theft_tilt_system_control_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6623] = "Tire_pressure_monitor_antenna_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6624] = "Heated_windshield_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6625] = "Rear_light_left_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6626] = "Ceiling_light_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6627
] = "Left_front_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6628
] = "Right_front_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6629
] = "Control_module_for_auxiliary_air_heater_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x662A] = "Belt Pretensioner left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x662B] = "Belt Pretensioner right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x662C] = "Occupant Detection_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x662D] = "Selector_lever_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x662E] = "NOx_sensor_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x662F] = "NOx_sensor_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6630] = "Ioniser_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6631
] = "Multi_function_steering_wheel_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6632] = "Left_rear_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6633] = "Right_rear_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6634
] = "Left_rear_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6635
] = "Right_rear_massage_seat_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6636
] = "Display_unit_1_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6637] = "Battery_monitoring_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6638] = "Roof_blind_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6639] = "Sun_roof_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x663A] = "Steering_angle_sender_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x663B] = "Lane_change_assistant 2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x663C] = "Pitch_rate_sender_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x663D] = "ESP_sensor_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x663E] = "Electronic_ignition_lock_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x663F] = "Air_quality_sensor_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6640
] = "Display_unit_2_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6641] = "Telephone_handset_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6642] = "Chip_card_reader_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6643] = "Traffic_data_aerial_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6644] = "Hands_free_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6645] = "Telephone_handset_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6646
] = "Display_unit_front_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6647] = "Multimedia_operating_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6648
] = "Digital_sound_system_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6649
] = "Electrically_adjustable_steering_column_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x664A
] = "Interface_for_external_multimedia_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x664B
] = "Relative_Air_Humidity_Interior_Sender_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x664C] = "Drivers_door_rear_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x664D] = "Passengers_rear_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x664E] = "Sensor_controlled_power_rear_lid_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x664F] = "Camera_for_night_vision_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6650
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6651] = "Rear_spoiler_adjustment_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6652] = "Roof_blind_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6653] = "Motor_for_wind_deflector_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6654] = "Voltage_stabilizer_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6655] = "Switch_module_for_driver_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6656
] = "Switch_module_for_front_passenger_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6657
] = "Switch_module_for_rear_seat_driver_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6658
] = "Switch_module_for_rear_seat_front_passenger_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6659] = "Switch_module_2_for_driver_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x665A] = "Battery_charger_unit_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x665B] = "Battery_charger_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x665C] = "Battery_charger_unit_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x665D] = "Air_conditioning_compressor_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x665E] = "Neck_heating_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x665F] = "Neck_heating_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6660
] = "Switch_module_2_for_front_passenger_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6661
] = "Switch_module_2_for_rear_seat_front_passenger_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6662] = "Compact_disc_database_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6663
] = "Rear_climatronic_operating_and_display_unit_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6664
] = "Rear_climatronic_operating_and_display_unit_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6665] = "Door_handle_front_left_Kessy_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6666] = "Door_handle_front_right_Kessy_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6667] = "Door_handle_rear_left_Kessy_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6668] = "Door_handle_rear_right_Kessy_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6669] = "Power_converter_DC_AC_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x666A] = "Battery_monitoring_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x666B] = "Matrix_headlamp_powermodule_1_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x666C] = "Matrix_headlamp_powermodule_1_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x666D] = "High_beam_powermodule_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x666E] = "High_beam_powermodule_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x666F] = "Air_suspension_compressor_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6670] = "Rear_brake_actuator_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6671] = "Rear_brake_actuator_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6672] = "Analog_clock_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6673] = "Rear_door_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6679] = "Data_medium_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x667A] = "Operating_unit_center_console_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x667B] = "Operating_unit_center_console_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x667C] = "Operating_unit_center_console_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x667D] = "Operating_unit_center_console_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x667E] = "Interface_for_radiodisplay_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x667F] = "Parkassist_entry_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6686] = "Belt_pretensioner_3rd_row_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6687] = "Belt_pretensioner_3rd_row_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6688] = "Injection_valve_heater_control_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6689] = "Steering_column_switch_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x668A] = "Brake_assistance_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x668B] = "Trailer_articulation_angle_sensor_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x668C
] = "Cup_holder_with_heater_and_cooling_element_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x668D] = "Range_of_vision_sensing_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x668E
] = "Convenience_and_driver_assist_operating_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x668F
] = "Cradle_rear_climatronic_operating_and_display_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6690
] = "Trailer_weight_nose_weight_detection_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6691] = "Sensor_carbon_dioxide_concentration_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6692] = "Sensor_fine_dust_concentration_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6693] = "Volume_control_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6694] = "Belt_buckle_presenter_2nd_row_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6695] = "Belt_buckle_presenter_2nd_row_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6696
] = "Operating_and_display_unit_6_for_air_conditioning_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6697] = "Active_accelerator_pedal_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6698] = "Multimedia_operating_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6699
] = "Display_unit_3_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x669A
] = "Display_unit_4_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x669B
] = "Display_unit_5_for_multimedia_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x669C
] = "Control_module_for_auxiliary_blower_motors_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x669D] = "Operating_and_display_unit_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x669E] = "Operating_and_display_unit_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x669F] = "Operating_and_display_unit_5_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A0] = "Side Sensor Driver Front_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A1] = "Side Sensor Passenger Front_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A2] = "Side Sensor Driver Rear_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A3] = "Side Sensor Passenger Rear_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A4] = "Front Sensor Driver_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A5] = "Front Sensor Passenger_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A6] = "Pedestrian Protection Driver_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A7] = "Pedestrian Protection Passenger_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A8] = "Rear Sensor Center_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66A9] = "Pedestrian Protection Center_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66AA] = "Pedestrian Protection Contact_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66AB] = "Pedestrian_protection_driver_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66AC] = "Pedestrian_protection_passenger_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66AD] = "Central_sensor_XY_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66AE
] = "Refrigerant_pressure_and_temperature_sender_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66AF
] = "Refrigerant_pressure_and_temperature_sender_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B0
] = "Switch_for_rear_multicontour_seat_driver_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B1
] = "Valve_block_1_in_driver_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B2
] = "Valve_block_2_in_driver_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B3
] = "Valve_block_3_in_driver_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B4
] = "Switch_for_rear_multicontour_seat_passenger_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B5
] = "Valve_block_1_in_passenger_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B6
] = "Valve_block_2_in_passenger_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B7
] = "Valve_block_3_in_passenger_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B8
] = "Switch_for_front_multicontour_seat_driver_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66B9
] = "Valve_block_1_in_driver_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66BA
] = "Valve_block_2_in_driver_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66BB
] = "Valve_block_3_in_driver_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66BC
] = "Switch_for_front_multicontour_seat_passenger_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66BD
] = "Valve_block_1_in_passenger_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66BE
] = "Valve_block_2_in_passenger_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66BF
] = "Valve_block_3_in_passenger_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C0] = "Coolant_heater_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C1] = "Seat_backrest_fan_1_front_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C2] = "Seat_backrest_fan_2_front_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C3] = "Seat_cushion_fan_1_front_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C4] = "Seat_cushion_fan_2_front_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C5] = "Seat_backrest_fan_1_front_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C6] = "Seat_backrest_fan_2_front_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C7] = "Seat_cushion_fan_1_front_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66C8] = "Seat_cushion_fan_2_front_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66C9
] = "Operating_and_display_unit_1_for_air_conditioning_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66CA
] = "Operating_and_display_unit_2_for_air_conditioning_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66CB
] = "Operating_and_display_unit_3_for_air_conditioning_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66CC
] = "Operating_and_display_unit_4_for_air_conditioning_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66CD
] = "Operating_and_display_unit_5_for_air_conditioning_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66CE
] = "Pedestrian_protection_left_hand_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66CF
] = "Pedestrian_protection_right_hand_side_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D0] = "Battery_junction_box_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D1] = "Cell_module_controller_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D2] = "Cell_module_controller_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D3] = "Cell_module_controller_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D4] = "Cell_module_controller_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D5] = "Cell_module_controller_5_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D6] = "Cell_module_controller_6_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D7] = "Cell_module_controller_7_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D8] = "Cell_module_controller_8_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66D9] = "Cell_module_controller_9_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66DA] = "Cell_module_controller_10_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66DB] = "Cell_module_controller_11_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66DC] = "Cell_module_controller_12_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66DD] = "Seat_backrest_fan_1_rear_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66DE] = "Seat_backrest_fan_2_rear_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66DF] = "Seat_cushion_fan_1_rear_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E0] = "Seat_cushion_fan_2_rear_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E1] = "Seat_backrest_fan_1_rear_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E2] = "Seat_backrest_fan_2_rear_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E3] = "Seat_cushion_fan_1_rear_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E4] = "Seat_cushion_fan_2_rear_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E5] = "Auxiliary_blower_motor_control_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E6] = "Auxiliary_blower_motor_control_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66E7
] = "Infrared_sender_for_front_observation_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66E8
] = "Starter_generator_control_module_sub_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66E9] = "Media_player_1_sub_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66EA] = "Media_player_2_sub_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66EB
] = "Dedicated_short_range_communication_aerial_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66EC
] = "Refrigerant_pressure_and_temperature_sender_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66ED
] = "Refrigerant_pressure_and_temperature_sender_5_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66EE
] = "Refrigerant_pressure_and_temperature_sender_6_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66EF] = "Air_coolant_actuator_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F0] = "Air_coolant_actuator_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F1] = "Cell_module_controller_13_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F2] = "Cell_module_controller_14_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F3] = "Cell_module_controller_15_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F5] = "Seat_heating_rear_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F6] = "LED_warning_indicator_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F7] = "Automatic_transmission_fluid_pump_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66F8] = "Manual_transmission_fluid_pump_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66F9
] = "Convenience_and_driver_assist_operating_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x66FB] = "Air_coolant_actuator_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66FC
] = "Valve_block_4_in_driver_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66FD
] = "Valve_block_4_in_passenger_side_rear_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66FE
] = "Valve_block_4_in_driver_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x66FF
] = "Valve_block_4_in_passenger_side_front_seat_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6701
] = "Rear_climatronic_operating_and_display_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6702] = "Refrigerant_expansion_valve_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6703] = "Refrigerant_expansion_valve_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6704] = "Refrigerant_expansion_valve_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6705] = "Refrigerant_shut_off_valve_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6706] = "Refrigerant_shut_off_valve_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6707] = "Refrigerant_shut_off_valve_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6708] = "Refrigerant_shut_off_valve_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6709] = "Refrigerant_shut_off_valve_5_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x670A] = "Sunlight_sensor_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x670B
] = "Near_field_communication_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x670C] = "Clutch_control_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x670D] = "Electrical_charger_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x670E] = "Rear_light_left_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x670F] = "Rear_light_right_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6710] = "Rear_light_right_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6711] = "Sunlight_sensor_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6712] = "Radiator_shutter_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6713] = "Radiator_shutter_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6714] = "Radiator_shutter_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6715] = "Radiator_shutter_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6718] = "Special_key_operating_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6719] = "Radio_interface_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x671A] = "Video_self_protection_recorder_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x671B] = "Special_vehicle_assist_interface_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x671C] = "Electric_system_disconnection_diode_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x671D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x671E] = "Belt_pretensioner_2nd_row_left_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x671F] = "Belt_pretensioner_2nd_row_right_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6720
] = "Electrical_variable_camshaft_phasing_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6721
] = "Electrical_variable_camshaft_phasing_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6722] = "Wireless_operating_unit_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6723] = "Wireless_operating_unit_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6724] = "Front_windshield_washer_pump_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6725] = "Air_quality_sensor_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6726] = "Fragrancing_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6727] = "Coolant_valve_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6728
] = "Near_field_communication_control_module_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6729] = "Interior_monitoring_rear_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x672A] = "Cooler_fan_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x672B] = "Control_unit_heating_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x672C] = "Control_unit_heating_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x672D] = "Control_unit_heating_3_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x672E] = "Control_unit_heating_4_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x672F] = "Operating_unit_drive_mode_selection_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6730] = "Side_sensor_a-pillar_driver_front_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6731
] = "Side_sensor_a-pillar_passenger_front_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6732] = "Sensor_high_voltage_system_1_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6733] = "Side_sensor_b-pillar_driver_front_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6734
] = "Side_sensor_b-pillar_passenger_front_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6735
] = "Multi_function_steering_wheel_control_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6736] = "Gear_selection_display_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6737] = "Cooler_fan_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6738] = "Gear_selector_control_module_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6739] = "Interior_light_module_2_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x673A] = "Radio_control_center_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x673B] = "Multimedia_extension_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x673C] = "Control_unit_differential_lock_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x673D] = "Control_unit_ride_control_system_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x673E
] = "Control_unit_hands_on_detection_steering_wheel_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x673F
] = "Front_climatronic_operating_and_display_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6740] = "Auxiliary_display_unit_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6741] = "Card_reader_tv_tuner_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6742] = "Park_lock_actuator_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6743] = "Media_connector_Hardware_Number"
UDS_RDBI.dataIdentifiers[0x6744] = "Catalyst_heating_Hardware_Number"
UDS_RDBI.dataIdentifiers[
    0x6801
] = "Control_unit_for_wiper_motor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6802
] = "Rain_light_recognition_sensor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6803] = "Light_switch_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6804
] = "Garage_door_opener_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6805
] = "Garage_door_opener_operating_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6806] = "Ignition_key_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6807
] = "Left_front_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6808
] = "Right_front_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6809
] = "Left_rear_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x680A
] = "LED_headlamp_powermodule_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x680B
] = "LED_headlamp_powermodule_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x680C
] = "LED_headlamp_powermodule_2_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x680D
] = "LED_headlamp_powermodule_2_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x680E
] = "Operating_and_display_unit_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x680F
] = "Operating_and_display_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6810
] = "Right_rear_seat_ventilation_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6811] = "Data_medium_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6812] = "Drivers_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6813
] = "Front_passengers_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6814
] = "Left_headlamp_power_output_stage_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6815
] = "Right_headlamp_power_output_stage_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6816
] = "Sensor_for_anti_theft_alarm_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6817] = "Rear_lid_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6818] = "Alarm_horn_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6819
] = "Automatic_day_night_interior_mirror_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x681A
] = "Remote_control_auxiliary_heater_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x681B] = "Fresh_air_blower_front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x681C] = "Fresh_air_blower_back_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x681D] = "Alternator_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x681E] = "Interior_light_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x681F
] = "Refrigerant_pressure_and_temperature_sender_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6820] = "Sun_roof_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6821
] = "Steering_column_lock_actuator_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6822
] = "Anti_theft_tilt_system_control_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6823
] = "Tire_pressure_monitor_antenna_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6824
] = "Heated_windshield_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6825] = "Rear_light_left_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6826] = "Ceiling_light_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6827
] = "Left_front_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6828
] = "Right_front_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6829
] = "Control_module_for_auxiliary_air_heater_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x682A] = "Belt Pretensioner left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x682B] = "Belt Pretensioner right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x682C] = "Occupant Detection_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x682D] = "Selector_lever_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x682E] = "NOx_sensor_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x682F] = "NOx_sensor_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6830] = "Ioniser_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6831
] = "Multi_function_steering_wheel_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6832
] = "Left_rear_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6833
] = "Right_rear_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6834
] = "Left_rear_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6835
] = "Right_rear_massage_seat_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6836
] = "Display_unit_1_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6837
] = "Battery_monitoring_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6838] = "Roof_blind_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6839] = "Sun_roof_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x683A] = "Steering_angle_sender_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x683B] = "Lane_change_assistant 2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x683C] = "Pitch_rate_sender_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x683D] = "ESP_sensor_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x683E] = "Electronic_ignition_lock_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x683F] = "Air_quality_sensor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6840
] = "Display_unit_2_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6841] = "Telephone_handset_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6842
] = "Chip_card_reader_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6843] = "Traffic_data_aerial_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6844] = "Hands_free_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6845] = "Telephone_handset_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6846
] = "Display_unit_front_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6847] = "Multimedia_operating_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6848
] = "Digital_sound_system_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6849
] = "Electrically_adjustable_steering_column_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x684A
] = "Interface_for_external_multimedia_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x684B
] = "Relative_Air_Humidity_Interior_Sender_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x684C
] = "Drivers_door_rear_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x684D
] = "Passengers_rear_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x684E
] = "Sensor_controlled_power_rear_lid_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x684F
] = "Camera_for_night_vision_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6850
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6851] = "Rear_spoiler_adjustment_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6852] = "Roof_blind_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6853] = "Motor_for_wind_deflector_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6854] = "Voltage_stabilizer_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6855
] = "Switch_module_for_driver_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6856
] = "Switch_module_for_front_passenger_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6857
] = "Switch_module_for_rear_seat_driver_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6858
] = "Switch_module_for_rear_seat_front_passenger_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6859
] = "Switch_module_2_for_driver_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x685A] = "Battery_charger_unit_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x685B] = "Battery_charger_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x685C] = "Battery_charger_unit_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x685D] = "Air_conditioning_compressor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x685E] = "Neck_heating_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x685F] = "Neck_heating_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6860
] = "Switch_module_2_for_front_passenger_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6861
] = "Switch_module_2_for_rear_seat_front_passenger_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6862] = "Compact_disc_database_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6863
] = "Rear_climatronic_operating_and_display_unit_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6864
] = "Rear_climatronic_operating_and_display_unit_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6865
] = "Door_handle_front_left_Kessy_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6866
] = "Door_handle_front_right_Kessy_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6867] = "Door_handle_rear_left_Kessy_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6868
] = "Door_handle_rear_right_Kessy_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6869] = "Power_converter_DC_AC_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x686A
] = "Battery_monitoring_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x686B
] = "Matrix_headlamp_powermodule_1_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x686C
] = "Matrix_headlamp_powermodule_1_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x686D] = "High_beam_powermodule_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x686E] = "High_beam_powermodule_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x686F] = "Air_suspension_compressor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6870] = "Rear_brake_actuator_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6871] = "Rear_brake_actuator_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6872] = "Analog_clock_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6873] = "Rear_door_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6879] = "Data_medium_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x687A
] = "Operating_unit_center_console_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x687B
] = "Operating_unit_center_console_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x687C
] = "Operating_unit_center_console_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x687D
] = "Operating_unit_center_console_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x687E] = "Interface_for_radiodisplay_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x687F] = "Parkassist_entry_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6886
] = "Belt_pretensioner_3rd_row_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6887
] = "Belt_pretensioner_3rd_row_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6888
] = "Injection_valve_heater_control_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6889] = "Steering_column_switch_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x688A] = "Brake_assistance_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x688B
] = "Trailer_articulation_angle_sensor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x688C
] = "Cup_holder_with_heater_and_cooling_element_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x688D] = "Range_of_vision_sensing_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x688E
] = "Convenience_and_driver_assist_operating_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x688F
] = "Cradle_rear_climatronic_operating_and_display_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6890
] = "Trailer_weight_nose_weight_detection_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6891
] = "Sensor_carbon_dioxide_concentration_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6892
] = "Sensor_fine_dust_concentration_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6893] = "Volume_control_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6894
] = "Belt_buckle_presenter_2nd_row_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6895
] = "Belt_buckle_presenter_2nd_row_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6896
] = "Operating_and_display_unit_6_for_air_conditioning_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6897] = "Active_accelerator_pedal_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6898] = "Multimedia_operating_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6899
] = "Display_unit_3_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x689A
] = "Display_unit_4_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x689B
] = "Display_unit_5_for_multimedia_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x689C
] = "Control_module_for_auxiliary_blower_motors_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x689D
] = "Operating_and_display_unit_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x689E
] = "Operating_and_display_unit_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x689F
] = "Operating_and_display_unit_5_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A0] = "Side Sensor Driver Front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A1] = "Side Sensor Passenger Front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A2] = "Side Sensor Driver Rear_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A3] = "Side Sensor Passenger Rear_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A4] = "Front Sensor Driver_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A5] = "Front Sensor Passenger_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68A6
] = "Pedestrian Protection Driver_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68A7
] = "Pedestrian Protection Passenger_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68A8] = "Rear Sensor Center_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68A9
] = "Pedestrian Protection Center_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68AA
] = "Pedestrian Protection Contact_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68AB
] = "Pedestrian_protection_driver_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68AC
] = "Pedestrian_protection_passenger_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68AD] = "Central_sensor_XY_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68AE
] = "Refrigerant_pressure_and_temperature_sender_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68AF
] = "Refrigerant_pressure_and_temperature_sender_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B0
] = "Switch_for_rear_multicontour_seat_driver_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B1
] = "Valve_block_1_in_driver_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B2
] = "Valve_block_2_in_driver_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B3
] = "Valve_block_3_in_driver_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B4
] = "Switch_for_rear_multicontour_seat_passenger_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B5
] = "Valve_block_1_in_passenger_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B6
] = "Valve_block_2_in_passenger_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B7
] = "Valve_block_3_in_passenger_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B8
] = "Switch_for_front_multicontour_seat_driver_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68B9
] = "Valve_block_1_in_driver_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68BA
] = "Valve_block_2_in_driver_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68BB
] = "Valve_block_3_in_driver_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68BC
] = "Switch_for_front_multicontour_seat_passenger_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68BD
] = "Valve_block_1_in_passenger_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68BE
] = "Valve_block_2_in_passenger_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68BF
] = "Valve_block_3_in_passenger_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68C0] = "Coolant_heater_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C1
] = "Seat_backrest_fan_1_front_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C2
] = "Seat_backrest_fan_2_front_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C3
] = "Seat_cushion_fan_1_front_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C4
] = "Seat_cushion_fan_2_front_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C5
] = "Seat_backrest_fan_1_front_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C6
] = "Seat_backrest_fan_2_front_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C7
] = "Seat_cushion_fan_1_front_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C8
] = "Seat_cushion_fan_2_front_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68C9
] = "Operating_and_display_unit_1_for_air_conditioning_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68CA
] = "Operating_and_display_unit_2_for_air_conditioning_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68CB
] = "Operating_and_display_unit_3_for_air_conditioning_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68CC
] = "Operating_and_display_unit_4_for_air_conditioning_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68CD
] = "Operating_and_display_unit_5_for_air_conditioning_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68CE
] = "Pedestrian_protection_left_hand_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68CF
] = "Pedestrian_protection_right_hand_side_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D0] = "Battery_junction_box_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D1] = "Cell_module_controller_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D2] = "Cell_module_controller_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D3] = "Cell_module_controller_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D4] = "Cell_module_controller_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D5] = "Cell_module_controller_5_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D6] = "Cell_module_controller_6_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D7] = "Cell_module_controller_7_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D8] = "Cell_module_controller_8_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68D9] = "Cell_module_controller_9_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68DA] = "Cell_module_controller_10_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68DB] = "Cell_module_controller_11_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68DC] = "Cell_module_controller_12_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68DD
] = "Seat_backrest_fan_1_rear_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68DE
] = "Seat_backrest_fan_2_rear_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68DF
] = "Seat_cushion_fan_1_rear_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E0
] = "Seat_cushion_fan_2_rear_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E1
] = "Seat_backrest_fan_1_rear_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E2
] = "Seat_backrest_fan_2_rear_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E3
] = "Seat_cushion_fan_1_rear_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E4
] = "Seat_cushion_fan_2_rear_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E5
] = "Auxiliary_blower_motor_control_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E6
] = "Auxiliary_blower_motor_control_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E7
] = "Infrared_sender_for_front_observation_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68E8
] = "Starter_generator_control_module_sub_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68E9] = "Media_player_1_sub_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68EA] = "Media_player_2_sub_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68EB
] = "Dedicated_short_range_communication_aerial_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68EC
] = "Refrigerant_pressure_and_temperature_sender_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68ED
] = "Refrigerant_pressure_and_temperature_sender_5_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68EE
] = "Refrigerant_pressure_and_temperature_sender_6_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68EF] = "Air_coolant_actuator_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68F0] = "Air_coolant_actuator_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68F1] = "Cell_module_controller_13_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68F2] = "Cell_module_controller_14_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68F3] = "Cell_module_controller_15_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68F5] = "Seat_heating_rear_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68F6] = "LED_warning_indicator_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68F7
] = "Automatic_transmission_fluid_pump_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68F8
] = "Manual_transmission_fluid_pump_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68F9
] = "Convenience_and_driver_assist_operating_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x68FB] = "Air_coolant_actuator_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68FC
] = "Valve_block_4_in_driver_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68FD
] = "Valve_block_4_in_passenger_side_rear_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68FE
] = "Valve_block_4_in_driver_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x68FF
] = "Valve_block_4_in_passenger_side_front_seat_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6901
] = "Rear_climatronic_operating_and_display_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6902
] = "Refrigerant_expansion_valve_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6903
] = "Refrigerant_expansion_valve_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6904
] = "Refrigerant_expansion_valve_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6905
] = "Refrigerant_shut_off_valve_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6906
] = "Refrigerant_shut_off_valve_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6907
] = "Refrigerant_shut_off_valve_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6908
] = "Refrigerant_shut_off_valve_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6909
] = "Refrigerant_shut_off_valve_5_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x690A] = "Sunlight_sensor_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x690B
] = "Near_field_communication_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x690C] = "Clutch_control_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x690D] = "Electrical_charger_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x690E] = "Rear_light_left_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x690F] = "Rear_light_right_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6910] = "Rear_light_right_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6911] = "Sunlight_sensor_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6912] = "Radiator_shutter_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6913] = "Radiator_shutter_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6914] = "Radiator_shutter_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6915] = "Radiator_shutter_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6918] = "Special_key_operating_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6919] = "Radio_interface_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x691A
] = "Video_self_protection_recorder_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x691B
] = "Special_vehicle_assist_interface_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x691C
] = "Electric_system_disconnection_diode_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x691D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x691E
] = "Belt_pretensioner_2nd_row_left_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x691F
] = "Belt_pretensioner_2nd_row_right_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6920
] = "Electrical_variable_camshaft_phasing_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6921
] = "Electrical_variable_camshaft_phasing_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6922] = "Wireless_operating_unit_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6923] = "Wireless_operating_unit_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6924
] = "Front_windshield_washer_pump_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6925] = "Air_quality_sensor_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6926] = "Fragrancing_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6927] = "Coolant_valve_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6928
] = "Near_field_communication_control_module_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6929] = "Interior_monitoring_rear_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x692A] = "Cooler_fan_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x692B] = "Control_unit_heating_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x692C] = "Control_unit_heating_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x692D] = "Control_unit_heating_3_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x692E] = "Control_unit_heating_4_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x692F
] = "Operating_unit_drive_mode_selection_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6930
] = "Side_sensor_a-pillar_driver_front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6931
] = "Side_sensor_a-pillar_passenger_front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6932
] = "Sensor_high_voltage_system_1_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6933
] = "Side_sensor_b-pillar_driver_front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6934
] = "Side_sensor_b-pillar_passenger_front_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6935
] = "Multi_function_steering_wheel_control_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6936] = "Gear_selection_display_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6937] = "Cooler_fan_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x6938
] = "Gear_selector_control_module_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6939] = "Interior_light_module_2_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x693A] = "Radio_control_center_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x693B] = "Multimedia_extension_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x693C
] = "Control_unit_differential_lock_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x693D
] = "Control_unit_ride_control_system_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x693E
] = "Control_unit_hands_on_detection_steering_wheel_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[
    0x693F
] = "Front_climatronic_operating_and_display_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6940] = "Auxiliary_display_unit_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6941] = "Card_reader_tv_tuner_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6942] = "Park_lock_actuator_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6943] = "Media_connector_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6944] = "Catalyst_heating_Hardware_Version_Number"
UDS_RDBI.dataIdentifiers[0x6A01] = "Control_unit_for_wiper_motor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A02] = "Rain_light_recognition_sensor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A03] = "Light_switch_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A04] = "Garage_door_opener_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A05] = "Garage_door_opener_operating_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A06] = "Ignition_key_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A07
] = "Left_front_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A08
] = "Right_front_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A09
] = "Left_rear_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A0A] = "LED_headlamp_powermodule_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A0B] = "LED_headlamp_powermodule_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A0C] = "LED_headlamp_powermodule_2_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A0D] = "LED_headlamp_powermodule_2_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A0E] = "Operating_and_display_unit_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A0F] = "Operating_and_display_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A10
] = "Right_rear_seat_ventilation_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A11] = "Data_medium_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A12] = "Drivers_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A13] = "Front_passengers_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A14] = "Left_headlamp_power_output_stage_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A15] = "Right_headlamp_power_output_stage_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A16] = "Sensor_for_anti_theft_alarm_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A17] = "Rear_lid_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A18] = "Alarm_horn_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A19] = "Automatic_day_night_interior_mirror_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A1A] = "Remote_control_auxiliary_heater_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A1B] = "Fresh_air_blower_front_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A1C] = "Fresh_air_blower_back_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A1D] = "Alternator_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A1E] = "Interior_light_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A1F
] = "Refrigerant_pressure_and_temperature_sender_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A20] = "Sun_roof_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A21] = "Steering_column_lock_actuator_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A22] = "Anti_theft_tilt_system_control_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A23] = "Tire_pressure_monitor_antenna_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A24] = "Heated_windshield_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A25] = "Rear_light_left_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A26] = "Ceiling_light_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A27
] = "Left_front_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A28
] = "Right_front_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A29
] = "Control_module_for_auxiliary_air_heater_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A2A] = "Belt Pretensioner left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A2B] = "Belt Pretensioner right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A2C] = "Occupant Detection_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A2D] = "Selector_lever_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A2E] = "NOx_sensor_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A2F] = "NOx_sensor_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A30] = "Ioniser_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A31
] = "Multi_function_steering_wheel_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A32] = "Left_rear_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A33] = "Right_rear_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A34] = "Left_rear_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A35
] = "Right_rear_massage_seat_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A36] = "Display_unit_1_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A37] = "Battery_monitoring_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A38] = "Roof_blind_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A39] = "Sun_roof_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A3A] = "Steering_angle_sender_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A3B] = "Lane_change_assistant 2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A3C] = "Pitch_rate_sender_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A3D] = "ESP_sensor_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A3E] = "Electronic_ignition_lock_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A3F] = "Air_quality_sensor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A40] = "Display_unit_2_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A41] = "Telephone_handset_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A42] = "Chip_card_reader_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A43] = "Traffic_data_aerial_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A44] = "Hands_free_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A45] = "Telephone_handset_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A46
] = "Display_unit_front_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A47] = "Multimedia_operating_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A48] = "Digital_sound_system_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A49
] = "Electrically_adjustable_steering_column_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A4A
] = "Interface_for_external_multimedia_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A4B] = "Relative_Air_Humidity_Interior_Sender_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A4C] = "Drivers_door_rear_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A4D] = "Passengers_rear_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A4E] = "Sensor_controlled_power_rear_lid_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A4F] = "Camera_for_night_vision_system_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A50
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A51] = "Rear_spoiler_adjustment_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A52] = "Roof_blind_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A53] = "Motor_for_wind_deflector_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A54] = "Voltage_stabilizer_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A55] = "Switch_module_for_driver_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A56
] = "Switch_module_for_front_passenger_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A57
] = "Switch_module_for_rear_seat_driver_side_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A58
] = "Switch_module_for_rear_seat_front_passenger_side_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A59] = "Switch_module_2_for_driver_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A5A] = "Battery_charger_unit_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A5B] = "Battery_charger_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A5C] = "Battery_charger_unit_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A5D] = "Air_conditioning_compressor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A5E] = "Neck_heating_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A5F] = "Neck_heating_right_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A60
] = "Switch_module_2_for_front_passenger_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A61
] = "Switch_module_2_for_rear_seat_front_passenger_side_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A62] = "Compact_disc_database_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A63
] = "Rear_climatronic_operating_and_display_unit_left_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A64
] = "Rear_climatronic_operating_and_display_unit_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A65] = "Door_handle_front_left_Kessy_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A66] = "Door_handle_front_right_Kessy_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A67] = "Door_handle_rear_left_Kessy_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A68] = "Door_handle_rear_right_Kessy_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A69] = "Power_converter_DC_AC_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A6A] = "Battery_monitoring_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A6B] = "Matrix_headlamp_powermodule_1_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A6C] = "Matrix_headlamp_powermodule_1_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A6D] = "High_beam_powermodule_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A6E] = "High_beam_powermodule_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A6F] = "Air_suspension_compressor_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A70] = "Rear_brake_actuator_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A71] = "Rear_brake_actuator_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A72] = "Analog_clock_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A73] = "Rear_door_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A79] = "Data_medium_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A7A] = "Operating_unit_center_console_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A7B] = "Operating_unit_center_console_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A7C] = "Operating_unit_center_console_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A7D] = "Operating_unit_center_console_4_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A7E] = "Interface_for_radiodisplay_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A7F] = "Parkassist_entry_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A86] = "Belt_pretensioner_3rd_row_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A87] = "Belt_pretensioner_3rd_row_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A88] = "Injection_valve_heater_control_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A89] = "Steering_column_switch_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A8A] = "Brake_assistance_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A8B] = "Trailer_articulation_angle_sensor_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A8C
] = "Cup_holder_with_heater_and_cooling_element_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A8D] = "Range_of_vision_sensing_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A8E
] = "Convenience_and_driver_assist_operating_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A8F
] = "Cradle_rear_climatronic_operating_and_display_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A90] = "Trailer_weight_nose_weight_detection_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A91] = "Sensor_carbon_dioxide_concentration_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A92] = "Sensor_fine_dust_concentration_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A93] = "Volume_control_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A94] = "Belt_buckle_presenter_2nd_row_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A95] = "Belt_buckle_presenter_2nd_row_right_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A96
] = "Operating_and_display_unit_6_for_air_conditioning_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A97] = "Active_accelerator_pedal_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A98] = "Multimedia_operating_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A99] = "Display_unit_3_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A9A] = "Display_unit_4_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A9B] = "Display_unit_5_for_multimedia_system_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6A9C
] = "Control_module_for_auxiliary_blower_motors_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A9D] = "Operating_and_display_unit_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A9E] = "Operating_and_display_unit_4_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6A9F] = "Operating_and_display_unit_5_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA0] = "Side Sensor Driver Front_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA1] = "Side Sensor Passenger Front_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA2] = "Side Sensor Driver Rear_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA3] = "Side Sensor Passenger Rear_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA4] = "Front Sensor Driver_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA5] = "Front Sensor Passenger_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA6] = "Pedestrian Protection Driver_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA7] = "Pedestrian Protection Passenger_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA8] = "Rear Sensor Center_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AA9] = "Pedestrian Protection Center_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AAA] = "Pedestrian Protection Contact_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AAB] = "Pedestrian_protection_driver_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AAC] = "Pedestrian_protection_passenger_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AAD] = "Central_sensor_XY_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AAE
] = "Refrigerant_pressure_and_temperature_sender_2_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AAF
] = "Refrigerant_pressure_and_temperature_sender_3_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB0
] = "Switch_for_rear_multicontour_seat_driver_side_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB1
] = "Valve_block_1_in_driver_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB2
] = "Valve_block_2_in_driver_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB3
] = "Valve_block_3_in_driver_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB4
] = "Switch_for_rear_multicontour_seat_passenger_side_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB5
] = "Valve_block_1_in_passenger_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB6
] = "Valve_block_2_in_passenger_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB7
] = "Valve_block_3_in_passenger_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB8
] = "Switch_for_front_multicontour_seat_driver_side_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AB9
] = "Valve_block_1_in_driver_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ABA
] = "Valve_block_2_in_driver_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ABB
] = "Valve_block_3_in_driver_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ABC
] = "Switch_for_front_multicontour_seat_passenger_side_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ABD
] = "Valve_block_1_in_passenger_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ABE
] = "Valve_block_2_in_passenger_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ABF
] = "Valve_block_3_in_passenger_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC0] = "Coolant_heater_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC1] = "Seat_backrest_fan_1_front_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC2] = "Seat_backrest_fan_2_front_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC3] = "Seat_cushion_fan_1_front_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC4] = "Seat_cushion_fan_2_front_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC5] = "Seat_backrest_fan_1_front_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC6] = "Seat_backrest_fan_2_front_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC7] = "Seat_cushion_fan_1_front_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AC8] = "Seat_cushion_fan_2_front_right_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AC9
] = "Operating_and_display_unit_1_for_air_conditioning_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ACA
] = "Operating_and_display_unit_2_for_air_conditioning_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ACB
] = "Operating_and_display_unit_3_for_air_conditioning_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ACC
] = "Operating_and_display_unit_4_for_air_conditioning_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6ACD
] = "Operating_and_display_unit_5_for_air_conditioning_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ACE] = "Pedestrian_protection_left_hand_side_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ACF] = "Pedestrian_protection_right_hand_side_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD0] = "Battery_junction_box_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD1] = "Cell_module_controller_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD2] = "Cell_module_controller_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD3] = "Cell_module_controller_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD4] = "Cell_module_controller_4_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD5] = "Cell_module_controller_5_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD6] = "Cell_module_controller_6_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD7] = "Cell_module_controller_7_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD8] = "Cell_module_controller_8_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AD9] = "Cell_module_controller_9_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ADA] = "Cell_module_controller_10_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ADB] = "Cell_module_controller_11_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ADC] = "Cell_module_controller_12_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ADD] = "Seat_backrest_fan_1_rear_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ADE] = "Seat_backrest_fan_2_rear_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6ADF] = "Seat_cushion_fan_1_rear_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE0] = "Seat_cushion_fan_2_rear_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE1] = "Seat_backrest_fan_1_rear_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE2] = "Seat_backrest_fan_2_rear_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE3] = "Seat_cushion_fan_1_rear_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE4] = "Seat_cushion_fan_2_rear_right_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE5] = "Auxiliary_blower_motor_control_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE6] = "Auxiliary_blower_motor_control_2_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AE7
] = "Infrared_sender_for_front_observation_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE8] = "Starter_generator_control_module_sub_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AE9] = "Media_player_1_sub_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AEA] = "Media_player_2_sub_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AEB
] = "Dedicated_short_range_communication_aerial_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AEC
] = "Refrigerant_pressure_and_temperature_sender_4_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AED
] = "Refrigerant_pressure_and_temperature_sender_5_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AEE
] = "Refrigerant_pressure_and_temperature_sender_6_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AEF] = "Air_coolant_actuator_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF0] = "Air_coolant_actuator_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF1] = "Cell_module_controller_13_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF2] = "Cell_module_controller_14_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF3] = "Cell_module_controller_15_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF5] = "Seat_heating_rear_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF6] = "LED_warning_indicator_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF7] = "Automatic_transmission_fluid_pump_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AF8] = "Manual_transmission_fluid_pump_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AF9
] = "Convenience_and_driver_assist_operating_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6AFB] = "Air_coolant_actuator_3_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AFC
] = "Valve_block_4_in_driver_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AFD
] = "Valve_block_4_in_passenger_side_rear_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AFE
] = "Valve_block_4_in_driver_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6AFF
] = "Valve_block_4_in_passenger_side_front_seat_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B01
] = "Rear_climatronic_operating_and_display_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B02] = "Refrigerant_expansion_valve_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B03] = "Refrigerant_expansion_valve_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B04] = "Refrigerant_expansion_valve_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B05] = "Refrigerant_shut_off_valve_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B06] = "Refrigerant_shut_off_valve_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B07] = "Refrigerant_shut_off_valve_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B08] = "Refrigerant_shut_off_valve_4_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B09] = "Refrigerant_shut_off_valve_5_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B0A] = "Sunlight_sensor_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B0B
] = "Near_field_communication_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B0C] = "Clutch_control_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B0D] = "Electrical_charger_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B0E] = "Rear_light_left_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B0F] = "Rear_light_right_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B10] = "Rear_light_right_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B11] = "Sunlight_sensor_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B12] = "Radiator_shutter_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B13] = "Radiator_shutter_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B14] = "Radiator_shutter_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B15] = "Radiator_shutter_4_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B18] = "Special_key_operating_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B19] = "Radio_interface_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B1A] = "Video_self_protection_recorder_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B1B] = "Special_vehicle_assist_interface_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B1C] = "Electric_system_disconnection_diode_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B1D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B1E] = "Belt_pretensioner_2nd_row_left_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B1F] = "Belt_pretensioner_2nd_row_right_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B20
] = "Electrical_variable_camshaft_phasing_1_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B21
] = "Electrical_variable_camshaft_phasing_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B22] = "Wireless_operating_unit_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B23] = "Wireless_operating_unit_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B24] = "Front_windshield_washer_pump_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B25] = "Air_quality_sensor_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B26] = "Fragrancing_system_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B27] = "Coolant_valve_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B28
] = "Near_field_communication_control_module_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B29] = "Interior_monitoring_rear_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B2A] = "Cooler_fan_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B2B] = "Control_unit_heating_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B2C] = "Control_unit_heating_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B2D] = "Control_unit_heating_3_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B2E] = "Control_unit_heating_4_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B2F] = "Operating_unit_drive_mode_selection_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B30] = "Side_sensor_a-pillar_driver_front_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B31] = "Side_sensor_a-pillar_passenger_front_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B32] = "Sensor_high_voltage_system_1_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B33] = "Side_sensor_b-pillar_driver_front_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B34] = "Side_sensor_b-pillar_passenger_front_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B35
] = "Multi_function_steering_wheel_control_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B36] = "Gear_selection_display_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B37] = "Cooler_fan_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B38] = "Gear_selector_control_module_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B39] = "Interior_light_module_2_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B3A] = "Radio_control_center_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B3B] = "Multimedia_extension_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B3C] = "Control_unit_differential_lock_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B3D] = "Control_unit_ride_control_system_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B3E
] = "Control_unit_hands_on_detection_steering_wheel_Serial_Number"
UDS_RDBI.dataIdentifiers[
    0x6B3F
] = "Front_climatronic_operating_and_display_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B40] = "Auxiliary_display_unit_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B41] = "Card_reader_tv_tuner_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B42] = "Park_lock_actuator_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B43] = "Media_connector_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6B44] = "Catalyst_heating_Serial_Number"
UDS_RDBI.dataIdentifiers[0x6C01] = "Control_unit_for_wiper_motor_System_Name"
UDS_RDBI.dataIdentifiers[0x6C02] = "Rain_light_recognition_sensor_System_Name"
UDS_RDBI.dataIdentifiers[0x6C03] = "Light_switch_System_Name"
UDS_RDBI.dataIdentifiers[0x6C04] = "Garage_door_opener_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C05] = "Garage_door_opener_operating_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C06] = "Ignition_key_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C07
] = "Left_front_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C08
] = "Right_front_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C09
] = "Left_rear_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C0A] = "LED_headlamp_powermodule_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C0B] = "LED_headlamp_powermodule_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C0C] = "LED_headlamp_powermodule_2_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C0D] = "LED_headlamp_powermodule_2_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C0E] = "Operating_and_display_unit_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C0F] = "Operating_and_display_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C10
] = "Right_rear_seat_ventilation_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C11] = "Data_medium_System_Name"
UDS_RDBI.dataIdentifiers[0x6C12] = "Drivers_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C13] = "Front_passengers_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C14] = "Left_headlamp_power_output_stage_System_Name"
UDS_RDBI.dataIdentifiers[0x6C15] = "Right_headlamp_power_output_stage_System_Name"
UDS_RDBI.dataIdentifiers[0x6C16] = "Sensor_for_anti_theft_alarm_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C17] = "Rear_lid_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C18] = "Alarm_horn_System_Name"
UDS_RDBI.dataIdentifiers[0x6C19] = "Automatic_day_night_interior_mirror_System_Name"
UDS_RDBI.dataIdentifiers[0x6C1A] = "Remote_control_auxiliary_heater_System_Name"
UDS_RDBI.dataIdentifiers[0x6C1B] = "Fresh_air_blower_front_System_Name"
UDS_RDBI.dataIdentifiers[0x6C1C] = "Fresh_air_blower_back_System_Name"
UDS_RDBI.dataIdentifiers[0x6C1D] = "Alternator_System_Name"
UDS_RDBI.dataIdentifiers[0x6C1E] = "Interior_light_module_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C1F
] = "Refrigerant_pressure_and_temperature_sender_System_Name"
UDS_RDBI.dataIdentifiers[0x6C20] = "Sun_roof_System_Name"
UDS_RDBI.dataIdentifiers[0x6C21] = "Steering_column_lock_actuator_System_Name"
UDS_RDBI.dataIdentifiers[0x6C22] = "Anti_theft_tilt_system_control_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C23] = "Tire_pressure_monitor_antenna_System_Name"
UDS_RDBI.dataIdentifiers[0x6C24] = "Heated_windshield_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C25] = "Rear_light_left_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C26] = "Ceiling_light_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C27] = "Left_front_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C28] = "Right_front_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C29] = "Control_module_for_auxiliary_air_heater_System_Name"
UDS_RDBI.dataIdentifiers[0x6C2A] = "Belt Pretensioner left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C2B] = "Belt Pretensioner right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C2C] = "Occupant Detection_System_Name"
UDS_RDBI.dataIdentifiers[0x6C2D] = "Selector_lever_System_Name"
UDS_RDBI.dataIdentifiers[0x6C2E] = "NOx_sensor_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C2F] = "NOx_sensor_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C30] = "Ioniser_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C31
] = "Multi_function_steering_wheel_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C32] = "Left_rear_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C33] = "Right_rear_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C34] = "Left_rear_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C35] = "Right_rear_massage_seat_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C36] = "Display_unit_1_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C37] = "Battery_monitoring_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C38] = "Roof_blind_System_Name"
UDS_RDBI.dataIdentifiers[0x6C39] = "Sun_roof_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C3A] = "Steering_angle_sender_System_Name"
UDS_RDBI.dataIdentifiers[0x6C3B] = "Lane_change_assistant 2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C3C] = "Pitch_rate_sender_System_Name"
UDS_RDBI.dataIdentifiers[0x6C3D] = "ESP_sensor_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C3E] = "Electronic_ignition_lock_System_Name"
UDS_RDBI.dataIdentifiers[0x6C3F] = "Air_quality_sensor_System_Name"
UDS_RDBI.dataIdentifiers[0x6C40] = "Display_unit_2_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C41] = "Telephone_handset_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C42] = "Chip_card_reader_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C43] = "Traffic_data_aerial_System_Name"
UDS_RDBI.dataIdentifiers[0x6C44] = "Hands_free_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C45] = "Telephone_handset_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C46
] = "Display_unit_front_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C47] = "Multimedia_operating_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C48] = "Digital_sound_system_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C49] = "Electrically_adjustable_steering_column_System_Name"
UDS_RDBI.dataIdentifiers[0x6C4A] = "Interface_for_external_multimedia_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C4B] = "Relative_Air_Humidity_Interior_Sender_System_Name"
UDS_RDBI.dataIdentifiers[0x6C4C] = "Drivers_door_rear_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C4D] = "Passengers_rear_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C4E] = "Sensor_controlled_power_rear_lid_System_Name"
UDS_RDBI.dataIdentifiers[0x6C4F] = "Camera_for_night_vision_system_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C50
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_System_Name"
UDS_RDBI.dataIdentifiers[0x6C51] = "Rear_spoiler_adjustment_System_Name"
UDS_RDBI.dataIdentifiers[0x6C52] = "Roof_blind_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C53] = "Motor_for_wind_deflector_System_Name"
UDS_RDBI.dataIdentifiers[0x6C54] = "Voltage_stabilizer_System_Name"
UDS_RDBI.dataIdentifiers[0x6C55] = "Switch_module_for_driver_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6C56] = "Switch_module_for_front_passenger_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6C57] = "Switch_module_for_rear_seat_driver_side_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C58
] = "Switch_module_for_rear_seat_front_passenger_side_System_Name"
UDS_RDBI.dataIdentifiers[0x6C59] = "Switch_module_2_for_driver_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6C5A] = "Battery_charger_unit_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C5B] = "Battery_charger_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C5C] = "Battery_charger_unit_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6C5D] = "Air_conditioning_compressor_System_Name"
UDS_RDBI.dataIdentifiers[0x6C5E] = "Neck_heating_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C5F] = "Neck_heating_right_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C60
] = "Switch_module_2_for_front_passenger_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C61
] = "Switch_module_2_for_rear_seat_front_passenger_side_System_Name"
UDS_RDBI.dataIdentifiers[0x6C62] = "Compact_disc_database_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C63
] = "Rear_climatronic_operating_and_display_unit_left_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C64
] = "Rear_climatronic_operating_and_display_unit_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C65] = "Door_handle_front_left_Kessy_System_Name"
UDS_RDBI.dataIdentifiers[0x6C66] = "Door_handle_front_right_Kessy_System_Name"
UDS_RDBI.dataIdentifiers[0x6C67] = "Door_handle_rear_left_Kessy_System_Name"
UDS_RDBI.dataIdentifiers[0x6C68] = "Door_handle_rear_right_Kessy_System_Name"
UDS_RDBI.dataIdentifiers[0x6C69] = "Power_converter_DC_AC_System_Name"
UDS_RDBI.dataIdentifiers[0x6C6A] = "Battery_monitoring_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C6B] = "Matrix_headlamp_powermodule_1_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C6C] = "Matrix_headlamp_powermodule_1_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C6D] = "High_beam_powermodule_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C6E] = "High_beam_powermodule_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C6F] = "Air_suspension_compressor_System_Name"
UDS_RDBI.dataIdentifiers[0x6C70] = "Rear_brake_actuator_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C71] = "Rear_brake_actuator_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C72] = "Analog_clock_System_Name"
UDS_RDBI.dataIdentifiers[0x6C73] = "Rear_door_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6C79] = "Data_medium_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C7A] = "Operating_unit_center_console_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C7B] = "Operating_unit_center_console_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C7C] = "Operating_unit_center_console_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6C7D] = "Operating_unit_center_console_4_System_Name"
UDS_RDBI.dataIdentifiers[0x6C7E] = "Interface_for_radiodisplay_System_Name"
UDS_RDBI.dataIdentifiers[0x6C7F] = "Parkassist_entry_System_Name"
UDS_RDBI.dataIdentifiers[0x6C86] = "Belt_pretensioner_3rd_row_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C87] = "Belt_pretensioner_3rd_row_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6C88] = "Injection_valve_heater_control_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C89] = "Steering_column_switch_System_Name"
UDS_RDBI.dataIdentifiers[0x6C8A] = "Brake_assistance_System_Name"
UDS_RDBI.dataIdentifiers[0x6C8B] = "Trailer_articulation_angle_sensor_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C8C
] = "Cup_holder_with_heater_and_cooling_element_System_Name"
UDS_RDBI.dataIdentifiers[0x6C8D] = "Range_of_vision_sensing_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C8E
] = "Convenience_and_driver_assist_operating_unit_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C8F
] = "Cradle_rear_climatronic_operating_and_display_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6C90] = "Trailer_weight_nose_weight_detection_System_Name"
UDS_RDBI.dataIdentifiers[0x6C91] = "Sensor_carbon_dioxide_concentration_System_Name"
UDS_RDBI.dataIdentifiers[0x6C92] = "Sensor_fine_dust_concentration_System_Name"
UDS_RDBI.dataIdentifiers[0x6C93] = "Volume_control_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6C94] = "Belt_buckle_presenter_2nd_row_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6C95] = "Belt_buckle_presenter_2nd_row_right_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C96
] = "Operating_and_display_unit_6_for_air_conditioning_System_Name"
UDS_RDBI.dataIdentifiers[0x6C97] = "Active_accelerator_pedal_System_Name"
UDS_RDBI.dataIdentifiers[0x6C98] = "Multimedia_operating_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6C99] = "Display_unit_3_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C9A] = "Display_unit_4_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6C9B] = "Display_unit_5_for_multimedia_system_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6C9C
] = "Control_module_for_auxiliary_blower_motors_System_Name"
UDS_RDBI.dataIdentifiers[0x6C9D] = "Operating_and_display_unit_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6C9E] = "Operating_and_display_unit_4_System_Name"
UDS_RDBI.dataIdentifiers[0x6C9F] = "Operating_and_display_unit_5_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA0] = "Side Sensor Driver Front_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA1] = "Side Sensor Passenger Front_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA2] = "Side Sensor Driver Rear_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA3] = "Side Sensor Passenger Rear_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA4] = "Front Sensor Driver_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA5] = "Front Sensor Passenger_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA6] = "Pedestrian Protection Driver_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA7] = "Pedestrian Protection Passenger_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA8] = "Rear Sensor Center_System_Name"
UDS_RDBI.dataIdentifiers[0x6CA9] = "Pedestrian Protection Center_System_Name"
UDS_RDBI.dataIdentifiers[0x6CAA] = "Pedestrian Protection Contact_System_Name"
UDS_RDBI.dataIdentifiers[0x6CAB] = "Pedestrian_protection_driver_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6CAC] = "Pedestrian_protection_passenger_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6CAD] = "Central_sensor_XY_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CAE
] = "Refrigerant_pressure_and_temperature_sender_2_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CAF
] = "Refrigerant_pressure_and_temperature_sender_3_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CB0
] = "Switch_for_rear_multicontour_seat_driver_side_System_Name"
UDS_RDBI.dataIdentifiers[0x6CB1] = "Valve_block_1_in_driver_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6CB2] = "Valve_block_2_in_driver_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6CB3] = "Valve_block_3_in_driver_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CB4
] = "Switch_for_rear_multicontour_seat_passenger_side_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CB5
] = "Valve_block_1_in_passenger_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CB6
] = "Valve_block_2_in_passenger_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CB7
] = "Valve_block_3_in_passenger_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CB8
] = "Switch_for_front_multicontour_seat_driver_side_System_Name"
UDS_RDBI.dataIdentifiers[0x6CB9] = "Valve_block_1_in_driver_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6CBA] = "Valve_block_2_in_driver_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6CBB] = "Valve_block_3_in_driver_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CBC
] = "Switch_for_front_multicontour_seat_passenger_side_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CBD
] = "Valve_block_1_in_passenger_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CBE
] = "Valve_block_2_in_passenger_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CBF
] = "Valve_block_3_in_passenger_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC0] = "Coolant_heater_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC1] = "Seat_backrest_fan_1_front_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC2] = "Seat_backrest_fan_2_front_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC3] = "Seat_cushion_fan_1_front_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC4] = "Seat_cushion_fan_2_front_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC5] = "Seat_backrest_fan_1_front_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC6] = "Seat_backrest_fan_2_front_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC7] = "Seat_cushion_fan_1_front_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CC8] = "Seat_cushion_fan_2_front_right_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CC9
] = "Operating_and_display_unit_1_for_air_conditioning_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CCA
] = "Operating_and_display_unit_2_for_air_conditioning_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CCB
] = "Operating_and_display_unit_3_for_air_conditioning_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CCC
] = "Operating_and_display_unit_4_for_air_conditioning_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CCD
] = "Operating_and_display_unit_5_for_air_conditioning_System_Name"
UDS_RDBI.dataIdentifiers[0x6CCE] = "Pedestrian_protection_left_hand_side_System_Name"
UDS_RDBI.dataIdentifiers[0x6CCF] = "Pedestrian_protection_right_hand_side_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD0] = "Battery_junction_box_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD1] = "Cell_module_controller_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD2] = "Cell_module_controller_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD3] = "Cell_module_controller_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD4] = "Cell_module_controller_4_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD5] = "Cell_module_controller_5_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD6] = "Cell_module_controller_6_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD7] = "Cell_module_controller_7_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD8] = "Cell_module_controller_8_System_Name"
UDS_RDBI.dataIdentifiers[0x6CD9] = "Cell_module_controller_9_System_Name"
UDS_RDBI.dataIdentifiers[0x6CDA] = "Cell_module_controller_10_System_Name"
UDS_RDBI.dataIdentifiers[0x6CDB] = "Cell_module_controller_11_System_Name"
UDS_RDBI.dataIdentifiers[0x6CDC] = "Cell_module_controller_12_System_Name"
UDS_RDBI.dataIdentifiers[0x6CDD] = "Seat_backrest_fan_1_rear_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CDE] = "Seat_backrest_fan_2_rear_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CDF] = "Seat_cushion_fan_1_rear_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE0] = "Seat_cushion_fan_2_rear_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE1] = "Seat_backrest_fan_1_rear_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE2] = "Seat_backrest_fan_2_rear_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE3] = "Seat_cushion_fan_1_rear_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE4] = "Seat_cushion_fan_2_rear_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE5] = "Auxiliary_blower_motor_control_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE6] = "Auxiliary_blower_motor_control_2_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CE7
] = "Infrared_sender_for_front_observation_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE8] = "Starter_generator_control_module_sub_System_Name"
UDS_RDBI.dataIdentifiers[0x6CE9] = "Media_player_1_sub_System_Name"
UDS_RDBI.dataIdentifiers[0x6CEA] = "Media_player_2_sub_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CEB
] = "Dedicated_short_range_communication_aerial_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CEC
] = "Refrigerant_pressure_and_temperature_sender_4_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CED
] = "Refrigerant_pressure_and_temperature_sender_5_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CEE
] = "Refrigerant_pressure_and_temperature_sender_6_System_Name"
UDS_RDBI.dataIdentifiers[0x6CEF] = "Air_coolant_actuator_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF0] = "Air_coolant_actuator_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF1] = "Cell_module_controller_13_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF2] = "Cell_module_controller_14_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF3] = "Cell_module_controller_15_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF5] = "Seat_heating_rear_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF6] = "LED_warning_indicator_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF7] = "Automatic_transmission_fluid_pump_System_Name"
UDS_RDBI.dataIdentifiers[0x6CF8] = "Manual_transmission_fluid_pump_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CF9
] = "Convenience_and_driver_assist_operating_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6CFB] = "Air_coolant_actuator_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6CFC] = "Valve_block_4_in_driver_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CFD
] = "Valve_block_4_in_passenger_side_rear_seat_System_Name"
UDS_RDBI.dataIdentifiers[0x6CFE] = "Valve_block_4_in_driver_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6CFF
] = "Valve_block_4_in_passenger_side_front_seat_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D01
] = "Rear_climatronic_operating_and_display_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6D02] = "Refrigerant_expansion_valve_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D03] = "Refrigerant_expansion_valve_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D04] = "Refrigerant_expansion_valve_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6D05] = "Refrigerant_shut_off_valve_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D06] = "Refrigerant_shut_off_valve_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D07] = "Refrigerant_shut_off_valve_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6D08] = "Refrigerant_shut_off_valve_4_System_Name"
UDS_RDBI.dataIdentifiers[0x6D09] = "Refrigerant_shut_off_valve_5_System_Name"
UDS_RDBI.dataIdentifiers[0x6D0A] = "Sunlight_sensor_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D0B
] = "Near_field_communication_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D0C] = "Clutch_control_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6D0D] = "Electrical_charger_System_Name"
UDS_RDBI.dataIdentifiers[0x6D0E] = "Rear_light_left_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D0F] = "Rear_light_right_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D10] = "Rear_light_right_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D11] = "Sunlight_sensor_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D12] = "Radiator_shutter_System_Name"
UDS_RDBI.dataIdentifiers[0x6D13] = "Radiator_shutter_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D14] = "Radiator_shutter_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6D15] = "Radiator_shutter_4_System_Name"
UDS_RDBI.dataIdentifiers[0x6D18] = "Special_key_operating_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6D19] = "Radio_interface_System_Name"
UDS_RDBI.dataIdentifiers[0x6D1A] = "Video_self_protection_recorder_System_Name"
UDS_RDBI.dataIdentifiers[0x6D1B] = "Special_vehicle_assist_interface_System_Name"
UDS_RDBI.dataIdentifiers[0x6D1C] = "Electric_system_disconnection_diode_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D1D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D1E] = "Belt_pretensioner_2nd_row_left_System_Name"
UDS_RDBI.dataIdentifiers[0x6D1F] = "Belt_pretensioner_2nd_row_right_System_Name"
UDS_RDBI.dataIdentifiers[0x6D20] = "Electrical_variable_camshaft_phasing_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D21] = "Electrical_variable_camshaft_phasing_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D22] = "Wireless_operating_unit_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D23] = "Wireless_operating_unit_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D24] = "Front_windshield_washer_pump_System_Name"
UDS_RDBI.dataIdentifiers[0x6D25] = "Air_quality_sensor_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D26] = "Fragrancing_system_System_Name"
UDS_RDBI.dataIdentifiers[0x6D27] = "Coolant_valve_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D28
] = "Near_field_communication_control_module_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6D29] = "Interior_monitoring_rear_System_Name"
UDS_RDBI.dataIdentifiers[0x6D2A] = "Cooler_fan_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D2B] = "Control_unit_heating_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D2C] = "Control_unit_heating_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D2D] = "Control_unit_heating_3_System_Name"
UDS_RDBI.dataIdentifiers[0x6D2E] = "Control_unit_heating_4_System_Name"
UDS_RDBI.dataIdentifiers[0x6D2F] = "Operating_unit_drive_mode_selection_System_Name"
UDS_RDBI.dataIdentifiers[0x6D30] = "Side_sensor_a-pillar_driver_front_System_Name"
UDS_RDBI.dataIdentifiers[0x6D31] = "Side_sensor_a-pillar_passenger_front_System_Name"
UDS_RDBI.dataIdentifiers[0x6D32] = "Sensor_high_voltage_system_1_System_Name"
UDS_RDBI.dataIdentifiers[0x6D33] = "Side_sensor_b-pillar_driver_front_System_Name"
UDS_RDBI.dataIdentifiers[0x6D34] = "Side_sensor_b-pillar_passenger_front_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D35
] = "Multi_function_steering_wheel_control_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D36] = "Gear_selection_display_System_Name"
UDS_RDBI.dataIdentifiers[0x6D37] = "Cooler_fan_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D38] = "Gear_selector_control_module_System_Name"
UDS_RDBI.dataIdentifiers[0x6D39] = "Interior_light_module_2_System_Name"
UDS_RDBI.dataIdentifiers[0x6D3A] = "Radio_control_center_System_Name"
UDS_RDBI.dataIdentifiers[0x6D3B] = "Multimedia_extension_System_Name"
UDS_RDBI.dataIdentifiers[0x6D3C] = "Control_unit_differential_lock_System_Name"
UDS_RDBI.dataIdentifiers[0x6D3D] = "Control_unit_ride_control_system_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D3E
] = "Control_unit_hands_on_detection_steering_wheel_System_Name"
UDS_RDBI.dataIdentifiers[
    0x6D3F
] = "Front_climatronic_operating_and_display_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6D40] = "Auxiliary_display_unit_System_Name"
UDS_RDBI.dataIdentifiers[0x6D41] = "Card_reader_tv_tuner_System_Name"
UDS_RDBI.dataIdentifiers[0x6D42] = "Park_lock_actuator_System_Name"
UDS_RDBI.dataIdentifiers[0x6D43] = "Media_connector_System_Name"
UDS_RDBI.dataIdentifiers[0x6D44] = "Catalyst_heating_System_Name"
UDS_RDBI.dataIdentifiers[0x6E01] = "Control_unit_for_wiper_motor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E02] = "Rain_light_recognition_sensor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E03] = "Light_switch_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E04
] = "Garage_door_opener_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E05
] = "Garage_door_opener_operating_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E06] = "Ignition_key_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E07
] = "Left_front_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E08
] = "Right_front_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E09
] = "Left_rear_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E0A] = "LED_headlamp_powermodule_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E0B
] = "LED_headlamp_powermodule_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E0C
] = "LED_headlamp_powermodule_2_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E0D
] = "LED_headlamp_powermodule_2_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E0E] = "Operating_and_display_unit_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E0F] = "Operating_and_display_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E10
] = "Right_rear_seat_ventilation_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E11] = "Data_medium_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E12] = "Drivers_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E13
] = "Front_passengers_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E14
] = "Left_headlamp_power_output_stage_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E15
] = "Right_headlamp_power_output_stage_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E16
] = "Sensor_for_anti_theft_alarm_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E17] = "Rear_lid_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E18] = "Alarm_horn_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E19
] = "Automatic_day_night_interior_mirror_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E1A
] = "Remote_control_auxiliary_heater_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E1B] = "Fresh_air_blower_front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E1C] = "Fresh_air_blower_back_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E1D] = "Alternator_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E1E] = "Interior_light_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E1F
] = "Refrigerant_pressure_and_temperature_sender_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E20] = "Sun_roof_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E21] = "Steering_column_lock_actuator_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E22
] = "Anti_theft_tilt_system_control_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E23] = "Tire_pressure_monitor_antenna_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E24
] = "Heated_windshield_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E25] = "Rear_light_left_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E26] = "Ceiling_light_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E27
] = "Left_front_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E28
] = "Right_front_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E29
] = "Control_module_for_auxiliary_air_heater_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E2A] = "Belt Pretensioner left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E2B] = "Belt Pretensioner right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E2C] = "Occupant Detection_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E2D] = "Selector_lever_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E2E] = "NOx_sensor_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E2F] = "NOx_sensor_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E30] = "Ioniser_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E31
] = "Multi_function_steering_wheel_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E32] = "Left_rear_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E33
] = "Right_rear_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E34
] = "Left_rear_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E35
] = "Right_rear_massage_seat_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E36
] = "Display_unit_1_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E37
] = "Battery_monitoring_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E38] = "Roof_blind_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E39] = "Sun_roof_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E3A] = "Steering_angle_sender_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E3B] = "Lane_change_assistant 2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E3C] = "Pitch_rate_sender_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E3D] = "ESP_sensor_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E3E] = "Electronic_ignition_lock_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E3F] = "Air_quality_sensor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E40
] = "Display_unit_2_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E41] = "Telephone_handset_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E42
] = "Chip_card_reader_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E43] = "Traffic_data_aerial_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E44] = "Hands_free_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E45] = "Telephone_handset_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E46
] = "Display_unit_front_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E47] = "Multimedia_operating_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E48
] = "Digital_sound_system_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E49
] = "Electrically_adjustable_steering_column_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E4A
] = "Interface_for_external_multimedia_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E4B
] = "Relative_Air_Humidity_Interior_Sender_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E4C
] = "Drivers_door_rear_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E4D
] = "Passengers_rear_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E4E
] = "Sensor_controlled_power_rear_lid_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E4F
] = "Camera_for_night_vision_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E50
] = "Relative_humidity_sensor_in_fresh_air_intake_duct_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E51] = "Rear_spoiler_adjustment_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E52] = "Roof_blind_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E53] = "Motor_for_wind_deflector_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E54] = "Voltage_stabilizer_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E55] = "Switch_module_for_driver_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E56
] = "Switch_module_for_front_passenger_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E57
] = "Switch_module_for_rear_seat_driver_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E58
] = "Switch_module_for_rear_seat_front_passenger_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E59
] = "Switch_module_2_for_driver_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E5A] = "Battery_charger_unit_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E5B] = "Battery_charger_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E5C] = "Battery_charger_unit_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E5D] = "Air_conditioning_compressor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E5E] = "Neck_heating_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E5F] = "Neck_heating_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E60
] = "Switch_module_2_for_front_passenger_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E61
] = "Switch_module_2_for_rear_seat_front_passenger_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E62] = "Compact_disc_database_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E63
] = "Rear_climatronic_operating_and_display_unit_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E64
] = "Rear_climatronic_operating_and_display_unit_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E65] = "Door_handle_front_left_Kessy_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E66] = "Door_handle_front_right_Kessy_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E67] = "Door_handle_rear_left_Kessy_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E68] = "Door_handle_rear_right_Kessy_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E69] = "Power_converter_DC_AC_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E6A
] = "Battery_monitoring_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E6B
] = "Matrix_headlamp_powermodule_1_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E6C
] = "Matrix_headlamp_powermodule_1_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E6D] = "High_beam_powermodule_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E6E] = "High_beam_powermodule_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E6F] = "Air_suspension_compressor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E70] = "Rear_brake_actuator_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E71] = "Rear_brake_actuator_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E72] = "Analog_clock_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E73] = "Rear_door_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E79] = "Data_medium_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E7A
] = "Operating_unit_center_console_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E7B
] = "Operating_unit_center_console_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E7C
] = "Operating_unit_center_console_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E7D
] = "Operating_unit_center_console_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E7E] = "Interface_for_radiodisplay_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E7F] = "Parkassist_entry_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E86
] = "Belt_pretensioner_3rd_row_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E87
] = "Belt_pretensioner_3rd_row_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E88
] = "Injection_valve_heater_control_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E89] = "Steering_column_switch_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E8A] = "Brake_assistance_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E8B
] = "Trailer_articulation_angle_sensor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E8C
] = "Cup_holder_with_heater_and_cooling_element_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E8D] = "Range_of_vision_sensing_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E8E
] = "Convenience_and_driver_assist_operating_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E8F
] = "Cradle_rear_climatronic_operating_and_display_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E90
] = "Trailer_weight_nose_weight_detection_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E91
] = "Sensor_carbon_dioxide_concentration_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E92
] = "Sensor_fine_dust_concentration_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E93] = "Volume_control_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E94
] = "Belt_buckle_presenter_2nd_row_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E95
] = "Belt_buckle_presenter_2nd_row_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E96
] = "Operating_and_display_unit_6_for_air_conditioning_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E97] = "Active_accelerator_pedal_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E98] = "Multimedia_operating_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E99
] = "Display_unit_3_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E9A
] = "Display_unit_4_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E9B
] = "Display_unit_5_for_multimedia_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6E9C
] = "Control_module_for_auxiliary_blower_motors_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E9D] = "Operating_and_display_unit_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E9E] = "Operating_and_display_unit_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6E9F] = "Operating_and_display_unit_5_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA0] = "Side Sensor Driver Front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA1] = "Side Sensor Passenger Front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA2] = "Side Sensor Driver Rear_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA3] = "Side Sensor Passenger Rear_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA4] = "Front Sensor Driver_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA5] = "Front Sensor Passenger_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA6] = "Pedestrian Protection Driver_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EA7
] = "Pedestrian Protection Passenger_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA8] = "Rear Sensor Center_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EA9] = "Pedestrian Protection Center_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EAA] = "Pedestrian Protection Contact_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EAB
] = "Pedestrian_protection_driver_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EAC
] = "Pedestrian_protection_passenger_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EAD] = "Central_sensor_XY_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EAE
] = "Refrigerant_pressure_and_temperature_sender_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EAF
] = "Refrigerant_pressure_and_temperature_sender_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB0
] = "Switch_for_rear_multicontour_seat_driver_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB1
] = "Valve_block_1_in_driver_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB2
] = "Valve_block_2_in_driver_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB3
] = "Valve_block_3_in_driver_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB4
] = "Switch_for_rear_multicontour_seat_passenger_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB5
] = "Valve_block_1_in_passenger_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB6
] = "Valve_block_2_in_passenger_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB7
] = "Valve_block_3_in_passenger_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB8
] = "Switch_for_front_multicontour_seat_driver_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EB9
] = "Valve_block_1_in_driver_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EBA
] = "Valve_block_2_in_driver_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EBB
] = "Valve_block_3_in_driver_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EBC
] = "Switch_for_front_multicontour_seat_passenger_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EBD
] = "Valve_block_1_in_passenger_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EBE
] = "Valve_block_2_in_passenger_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EBF
] = "Valve_block_3_in_passenger_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EC0] = "Coolant_heater_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC1
] = "Seat_backrest_fan_1_front_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC2
] = "Seat_backrest_fan_2_front_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EC3] = "Seat_cushion_fan_1_front_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EC4] = "Seat_cushion_fan_2_front_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC5
] = "Seat_backrest_fan_1_front_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC6
] = "Seat_backrest_fan_2_front_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC7
] = "Seat_cushion_fan_1_front_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC8
] = "Seat_cushion_fan_2_front_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EC9
] = "Operating_and_display_unit_1_for_air_conditioning_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6ECA
] = "Operating_and_display_unit_2_for_air_conditioning_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6ECB
] = "Operating_and_display_unit_3_for_air_conditioning_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6ECC
] = "Operating_and_display_unit_4_for_air_conditioning_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6ECD
] = "Operating_and_display_unit_5_for_air_conditioning_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6ECE
] = "Pedestrian_protection_left_hand_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6ECF
] = "Pedestrian_protection_right_hand_side_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED0] = "Battery_junction_box_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED1] = "Cell_module_controller_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED2] = "Cell_module_controller_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED3] = "Cell_module_controller_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED4] = "Cell_module_controller_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED5] = "Cell_module_controller_5_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED6] = "Cell_module_controller_6_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED7] = "Cell_module_controller_7_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED8] = "Cell_module_controller_8_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6ED9] = "Cell_module_controller_9_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EDA] = "Cell_module_controller_10_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EDB] = "Cell_module_controller_11_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EDC] = "Cell_module_controller_12_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EDD] = "Seat_backrest_fan_1_rear_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EDE] = "Seat_backrest_fan_2_rear_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EDF] = "Seat_cushion_fan_1_rear_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EE0] = "Seat_cushion_fan_2_rear_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EE1
] = "Seat_backrest_fan_1_rear_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EE2
] = "Seat_backrest_fan_2_rear_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EE3] = "Seat_cushion_fan_1_rear_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EE4] = "Seat_cushion_fan_2_rear_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EE5
] = "Auxiliary_blower_motor_control_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EE6
] = "Auxiliary_blower_motor_control_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EE7
] = "Infrared_sender_for_front_observation_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EE8
] = "Starter_generator_control_module_sub_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EE9] = "Media_player_1_sub_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EEA] = "Media_player_2_sub_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EEB
] = "Dedicated_short_range_communication_aerial_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EEC
] = "Refrigerant_pressure_and_temperature_sender_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EED
] = "Refrigerant_pressure_and_temperature_sender_5_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EEE
] = "Refrigerant_pressure_and_temperature_sender_6_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EEF] = "Air_coolant_actuator_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EF0] = "Air_coolant_actuator_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EF1] = "Cell_module_controller_13_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EF2] = "Cell_module_controller_14_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EF3] = "Cell_module_controller_15_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EF5] = "Seat_heating_rear_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EF6] = "LED_warning_indicator_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EF7
] = "Automatic_transmission_fluid_pump_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EF8
] = "Manual_transmission_fluid_pump_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EF9
] = "Convenience_and_driver_assist_operating_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6EFB] = "Air_coolant_actuator_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EFC
] = "Valve_block_4_in_driver_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EFD
] = "Valve_block_4_in_passenger_side_rear_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EFE
] = "Valve_block_4_in_driver_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6EFF
] = "Valve_block_4_in_passenger_side_front_seat_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F01
] = "Rear_climatronic_operating_and_display_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F02] = "Refrigerant_expansion_valve_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F03] = "Refrigerant_expansion_valve_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F04] = "Refrigerant_expansion_valve_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F05] = "Refrigerant_shut_off_valve_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F06] = "Refrigerant_shut_off_valve_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F07] = "Refrigerant_shut_off_valve_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F08] = "Refrigerant_shut_off_valve_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F09] = "Refrigerant_shut_off_valve_5_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F0A] = "Sunlight_sensor_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F0B
] = "Near_field_communication_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F0C] = "Clutch_control_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F0D] = "Electrical_charger_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F0E] = "Rear_light_left_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F0F] = "Rear_light_right_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F10] = "Rear_light_right_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F11] = "Sunlight_sensor_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F12] = "Radiator_shutter_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F13] = "Radiator_shutter_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F14] = "Radiator_shutter_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F15] = "Radiator_shutter_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F18] = "Special_key_operating_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F19] = "Radio_interface_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F1A
] = "Video_self_protection_recorder_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F1B
] = "Special_vehicle_assist_interface_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F1C
] = "Electric_system_disconnection_diode_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F1D
] = "Cradle_rear_climatronic_operating_and_display_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F1E
] = "Belt_pretensioner_2nd_row_left_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F1F
] = "Belt_pretensioner_2nd_row_right_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F20
] = "Electrical_variable_camshaft_phasing_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F21
] = "Electrical_variable_camshaft_phasing_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F22] = "Wireless_operating_unit_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F23] = "Wireless_operating_unit_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F24] = "Front_windshield_washer_pump_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F25] = "Air_quality_sensor_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F26] = "Fragrancing_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F27] = "Coolant_valve_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F28
] = "Near_field_communication_control_module_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F29] = "Interior_monitoring_rear_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F2A] = "Cooler_fan_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F2B] = "Control_unit_heating_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F2C] = "Control_unit_heating_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F2D] = "Control_unit_heating_3_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F2E] = "Control_unit_heating_4_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F2F
] = "Operating_unit_drive_mode_selection_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F30
] = "Side_sensor_a-pillar_driver_front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F31
] = "Side_sensor_a-pillar_passenger_front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F32] = "Sensor_high_voltage_system_1_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F33
] = "Side_sensor_b-pillar_driver_front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F34
] = "Side_sensor_b-pillar_passenger_front_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F35
] = "Multi_function_steering_wheel_control_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F36] = "Gear_selection_display_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F37] = "Cooler_fan_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F38] = "Gear_selector_control_module_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F39] = "Interior_light_module_2_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F3A] = "Radio_control_center_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F3B] = "Multimedia_extension_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F3C
] = "Control_unit_differential_lock_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F3D
] = "Control_unit_ride_control_system_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F3E
] = "Control_unit_hands_on_detection_steering_wheel_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[
    0x6F3F
] = "Front_climatronic_operating_and_display_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F40] = "Auxiliary_display_unit_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F41] = "Card_reader_tv_tuner_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F42] = "Park_lock_actuator_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F43] = "Media_connector_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0x6F44] = "Catalyst_heating_VW_Slave_FAZIT_string"
UDS_RDBI.dataIdentifiers[0xEF90] = "Secure_hardware_extension_status"
UDS_RDBI.dataIdentifiers[0xF15A] = "Fingerprint"
UDS_RDBI.dataIdentifiers[
    0xF15B
] = "Fingerprint And Programming Date Of Logical Software Blocks"
UDS_RDBI.dataIdentifiers[0xF17C] = "VW FAZIT Identification String"
UDS_RDBI.dataIdentifiers[0xF186] = "Active Diagnostic Session"
UDS_RDBI.dataIdentifiers[0xF187] = "VW Spare Part Number"
UDS_RDBI.dataIdentifiers[0xF189] = "VW Application Software Version Number"
UDS_RDBI.dataIdentifiers[0xF18A] = "System Supplier Identifier"
UDS_RDBI.dataIdentifiers[0xF18C] = "ECU Serial Number"
UDS_RDBI.dataIdentifiers[0xF190] = "Vehicle Identification Number"
UDS_RDBI.dataIdentifiers[0xF191] = "VW ECU Hardware Number"
UDS_RDBI.dataIdentifiers[0xF192] = "System Supplier ECU Hardware Number"
UDS_RDBI.dataIdentifiers[0xF193] = "System Supplier ECU Hardware Version Number"
UDS_RDBI.dataIdentifiers[0xF194] = "System Supplier ECU Software Number"
UDS_RDBI.dataIdentifiers[0xF195] = "System Supplier ECU Software Version Number"
UDS_RDBI.dataIdentifiers[0xF197] = "VW System Name Or Engine Type"
UDS_RDBI.dataIdentifiers[0xF19E] = "ASAM ODX File Identifier"
UDS_RDBI.dataIdentifiers[0xF1A0] = "VW Data Set Number Or ECU Data Container Number"
UDS_RDBI.dataIdentifiers[0xF1A1] = "VW Data Set Version Number"
UDS_RDBI.dataIdentifiers[0xF1A2] = "ASAM ODX File Version"
UDS_RDBI.dataIdentifiers[0xF1A3] = "VW ECU Hardware Version Number"
UDS_RDBI.dataIdentifiers[0xF1AA] = "VW Workshop System Name"
UDS_RDBI.dataIdentifiers[0xF1AB] = "VW Logical Software Block Version"
UDS_RDBI.dataIdentifiers[0xF1AD] = "Engine Code Letters"
UDS_RDBI.dataIdentifiers[
    0xF1AF
] = "AUTOSAR_standard_application_software_identification"
UDS_RDBI.dataIdentifiers[0xF1B0] = "VWClear_diagnostic_information_date_functional"
UDS_RDBI.dataIdentifiers[0xF1B1] = "VW_Application_data_set_identification"
UDS_RDBI.dataIdentifiers[0xF1B2] = "Function_software_identification"
UDS_RDBI.dataIdentifiers[0xF1B3] = "VW_Data_set_name"
UDS_RDBI.dataIdentifiers[0xF1B5] = "Busmaster_description"
UDS_RDBI.dataIdentifiers[0xF1B6] = "System_identification"
UDS_RDBI.dataIdentifiers[0xF1B7] = "Gateway_component_list_ECU_node_address"
UDS_RDBI.dataIdentifiers[0xF1D5] = "FDS_project_data"
UDS_RDBI.dataIdentifiers[0xF1DF] = "ECU Programming Information"


UDS_RC.routineControlTypes[0x0202] = "Check Memory"
UDS_RC.routineControlTypes[0x0203] = "Check Programming Preconditions"
UDS_RC.routineControlTypes[0x0317] = "Reset of Adaption Values"
UDS_RC.routineControlTypes[0x0366] = "Reset of all Adaptions"
UDS_RC.routineControlTypes[0x03E7] = "Reset to Factory Settings"
UDS_RC.routineControlTypes[0x045A] = "Clear user defined DTC information"
UDS_RC.routineControlTypes[0x0544] = "Verify partial software checksum"
UDS_RC.routineControlTypes[0x0594] = "Check upload preconditions"
UDS_RC.routineControlTypes[0xFF00] = "Erase Memory"
UDS_RC.routineControlTypes[0xFF01] = "Check Programming Dependencies"


UDS_RD.dataFormatIdentifiers[0x0000] = "Uncompressed"
UDS_RD.dataFormatIdentifiers[0x0001] = "Compression Method 1"
UDS_RD.dataFormatIdentifiers[0x0002] = "Compression Method 2"
UDS_RD.dataFormatIdentifiers[0x0003] = "Compression Method 3"
UDS_RD.dataFormatIdentifiers[0x0004] = "Compression Method 4"
UDS_RD.dataFormatIdentifiers[0x0005] = "Compression Method 5"
UDS_RD.dataFormatIdentifiers[0x0006] = "Compression Method 6"
UDS_RD.dataFormatIdentifiers[0x0007] = "Compression Method 7"
UDS_RD.dataFormatIdentifiers[0x0008] = "Compression Method 8"
UDS_RD.dataFormatIdentifiers[0x0009] = "Compression Method 9"
UDS_RD.dataFormatIdentifiers[0x000A] = "Compression Method 10"
UDS_RD.dataFormatIdentifiers[0x000B] = "Compression Method 11"
UDS_RD.dataFormatIdentifiers[0x000C] = "Compression Method 12"
UDS_RD.dataFormatIdentifiers[0x000D] = "Compression Method 13"
UDS_RD.dataFormatIdentifiers[0x000E] = "Compression Method 14"
UDS_RD.dataFormatIdentifiers[0x000F] = "Compression Method 15"
