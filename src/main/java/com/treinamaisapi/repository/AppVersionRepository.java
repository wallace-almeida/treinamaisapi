package com.treinamaisapi.repository;


import com.treinamaisapi.entity.appVersion.AppVersion;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AppVersionRepository extends JpaRepository<AppVersion, Long>, JpaSpecificationExecutor<AppVersion>,  PagingAndSortingRepository<AppVersion, Long>{

    Optional<AppVersion> findTopByOrderByVersionCodeDesc();



}
