package com.adontoApi.entity.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class allUserDto {
    private String name;
    private String email;
    private String phone;
    private String cpfUser;
    private String userType;
}
